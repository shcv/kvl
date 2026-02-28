const std = @import("std");
const Allocator = std.mem.Allocator;

// ---------------------------------------------------------------------------
// Data model
// ---------------------------------------------------------------------------

pub const Value = union(enum) {
    string: []const u8,
    object: *Object,
};

pub const Object = struct {
    entries: std.ArrayListUnmanaged(Entry) = .{},

    pub const Entry = struct {
        key: []const u8,
        value: Value,
    };

    pub fn findIndex(self: *const Object, key: []const u8) ?usize {
        for (self.entries.items, 0..) |entry, i| {
            if (std.mem.eql(u8, entry.key, key)) return i;
        }
        return null;
    }

    pub fn get(self: *const Object, key: []const u8) ?Value {
        if (self.findIndex(key)) |i| return self.entries.items[i].value;
        return null;
    }

    pub fn put(self: *Object, alloc: Allocator, key: []const u8, value: Value) !void {
        try self.entries.append(alloc, .{ .key = key, .value = value });
    }

    /// Merge a value under `key`. Handles repeated-key semantics:
    ///   string + string  → categorical object {old:{}, new:{}}
    ///   object + string  → add new as categorical entry
    ///   string + object  → wrap old string, merge new entries
    ///   object + object  → recursive merge
    pub fn merge(self: *Object, alloc: Allocator, key: []const u8, value: Value) !void {
        const idx = self.findIndex(key) orelse {
            try self.put(alloc, key, value);
            return;
        };
        const existing = &self.entries.items[idx].value;

        switch (existing.*) {
            .string => |old_str| switch (value) {
                .string => |new_str| {
                    // string + string → categorical {old:{}, new:{}}
                    const cat = try createObject(alloc);
                    try cat.put(alloc, old_str, .{ .object = try createObject(alloc) });
                    try cat.put(alloc, new_str, .{ .object = try createObject(alloc) });
                    existing.* = .{ .object = cat };
                },
                .object => |new_obj| {
                    // string + object → wrap string, merge object entries
                    const cat = try createObject(alloc);
                    try cat.put(alloc, old_str, .{ .object = try createObject(alloc) });
                    for (new_obj.entries.items) |entry| {
                        try cat.merge(alloc, entry.key, entry.value);
                    }
                    existing.* = .{ .object = cat };
                },
            },
            .object => |existing_obj| switch (value) {
                .string => |new_str| {
                    // object + string → add as categorical entry
                    try existing_obj.put(alloc, new_str, .{ .object = try createObject(alloc) });
                },
                .object => |new_obj| {
                    // object + object → recursive merge
                    for (new_obj.entries.items) |entry| {
                        try existing_obj.merge(alloc, entry.key, entry.value);
                    }
                },
            },
        }
    }
};

fn createObject(alloc: Allocator) !*Object {
    const obj = try alloc.create(Object);
    obj.* = .{};
    return obj;
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub const ParseError = error{
    MissingSeparator,
    OutOfMemory,
};

const StackEntry = struct {
    indent: i32,
    obj: *Object,
};

/// Parse KVL text into a categorical object tree.
/// All strings in the returned tree are slices into `input` (or allocated
/// copies when escape-sequence processing is needed).
pub fn parse(alloc: Allocator, input: []const u8) !*Object {
    const root = try createObject(alloc);

    var stack = std.ArrayListUnmanaged(StackEntry){};
    defer stack.deinit(alloc);
    try stack.append(alloc, .{ .indent = -1, .obj = root });

    var line_iter = std.mem.splitSequence(u8, input, "\n");
    while (line_iter.next()) |raw_line| {
        // Strip trailing \r for Windows line endings
        const line = std.mem.trimRight(u8, raw_line, "\r");

        // Skip blank lines
        if (isBlank(line)) continue;

        // Skip header lines
        if (isHeader(line)) continue;

        const indent = measureIndent(line);
        const content = line[@intCast(indent)..];

        // Pop stack back to find the correct parent
        while (stack.items.len > 1 and stack.items[stack.items.len - 1].indent >= indent) {
            _ = stack.pop();
        }
        const parent = stack.items[stack.items.len - 1].obj;

        // Find the separator (first unescaped '=')
        const sep_pos = findSeparator(content, '=') orelse continue;

        const raw_key = content[0..sep_pos];
        const raw_value = content[sep_pos + 1 ..];

        const key = trimWhitespace(raw_key);
        const val = trimWhitespace(raw_value);

        if (val.len == 0) {
            // Empty value → nested object follows
            const child = try getOrCreateNested(parent, alloc, key);
            try stack.append(alloc, .{ .indent = indent, .obj = child });
        } else {
            // Simple key-value
            try parent.merge(alloc, key, .{ .string = val });
        }
    }

    // Unescape separator patterns in all keys and string values
    try unescapeTree(alloc, root, '=');

    return root;
}

fn isBlank(line: []const u8) bool {
    for (line) |c| {
        if (c != ' ' and c != '\t') return false;
    }
    return true;
}

fn isHeader(line: []const u8) bool {
    const trimmed = std.mem.trimLeft(u8, line, " \t");
    return trimmed.len > 0 and trimmed[0] == '#';
}

/// Count leading whitespace as an indent level.
/// Tabs count as 4 spaces.
fn measureIndent(line: []const u8) i32 {
    var n: i32 = 0;
    for (line) |c| {
        if (c == ' ') {
            n += 1;
        } else if (c == '\t') {
            n += 4;
        } else {
            break;
        }
    }
    return n;
}

/// Find the first unescaped occurrence of `sep` in `s`.
/// A backslash before `sep` or before another backslash escapes it.
fn findSeparator(s: []const u8, sep: u8) ?usize {
    var i: usize = 0;
    while (i < s.len) {
        if (s[i] == '\\' and i + 1 < s.len) {
            if (s[i + 1] == sep or s[i + 1] == '\\') {
                i += 2;
                continue;
            }
            // Backslash before non-separator: literal, advance one
            i += 1;
            continue;
        }
        if (s[i] == sep) return i;
        i += 1;
    }
    return null;
}

fn trimWhitespace(s: []const u8) []const u8 {
    return std.mem.trim(u8, s, " \t");
}

/// Get an existing object for `key`, or create a new one.
/// If the key currently holds a string, convert it to a categorical entry.
fn getOrCreateNested(parent: *Object, alloc: Allocator, key: []const u8) !*Object {
    if (parent.findIndex(key)) |idx| {
        const existing = &parent.entries.items[idx].value;
        switch (existing.*) {
            .object => |obj| return obj,
            .string => |s| {
                const cat = try createObject(alloc);
                try cat.put(alloc, s, .{ .object = try createObject(alloc) });
                existing.* = .{ .object = cat };
                return cat;
            },
        }
    }
    const obj = try createObject(alloc);
    try parent.put(alloc, key, .{ .object = obj });
    return obj;
}

// ---------------------------------------------------------------------------
// Escape sequence processing
// ---------------------------------------------------------------------------

/// Unescape separator patterns in text: \<sep> becomes <sep>.
/// Returns the original slice unchanged if no escapes are found (no allocation).
fn unescapeText(alloc: Allocator, text: []const u8, sep: u8) ![]const u8 {
    // Quick check: does text contain \<sep>?
    var has_escape = false;
    var i: usize = 0;
    while (i < text.len) {
        if (text[i] == '\\' and i + 1 < text.len and text[i + 1] == sep) {
            has_escape = true;
            break;
        }
        i += 1;
    }
    if (!has_escape) return text;

    // Count result length
    var result_len: usize = 0;
    i = 0;
    while (i < text.len) {
        if (text[i] == '\\' and i + 1 < text.len and text[i + 1] == sep) {
            result_len += 1;
            i += 2;
        } else {
            result_len += 1;
            i += 1;
        }
    }

    // Build unescaped result
    const result = try alloc.alloc(u8, result_len);
    var j: usize = 0;
    i = 0;
    while (i < text.len) {
        if (text[i] == '\\' and i + 1 < text.len and text[i + 1] == sep) {
            result[j] = sep;
            j += 1;
            i += 2;
        } else {
            result[j] = text[i];
            j += 1;
            i += 1;
        }
    }

    return result;
}

/// Recursively unescape all keys and string values in the object tree.
fn unescapeTree(alloc: Allocator, obj: *Object, sep: u8) !void {
    for (obj.entries.items) |*entry| {
        entry.key = try unescapeText(alloc, entry.key, sep);
        switch (entry.value) {
            .string => |s| {
                entry.value = .{ .string = try unescapeText(alloc, s, sep) };
            },
            .object => |child| {
                try unescapeTree(alloc, child, sep);
            },
        }
    }
}

// ---------------------------------------------------------------------------
// Compacted JSON output
// ---------------------------------------------------------------------------

/// Write the parsed object tree as compacted JSON (pretty-printed).
/// Categorical structures (all children are empty objects) become arrays.
pub fn writeCompactedJson(writer: anytype, obj: *const Object) !void {
    try writeObjectJson(writer, obj, 0);
    try writer.writeByte('\n');
}

fn writeObjectJson(writer: anytype, obj: *const Object, depth: usize) anyerror!void {
    if (obj.entries.items.len == 0) {
        try writer.writeAll("{}");
        return;
    }
    try writer.writeAll("{\n");
    for (obj.entries.items, 0..) |entry, i| {
        try writeIndent(writer, depth + 1);
        try writeJsonString(writer, entry.key);
        try writer.writeAll(": ");
        try writeValueJson(writer, entry.value, depth + 1);
        if (i + 1 < obj.entries.items.len) {
            try writer.writeByte(',');
        }
        try writer.writeByte('\n');
    }
    try writeIndent(writer, depth);
    try writer.writeByte('}');
}

fn writeValueJson(writer: anytype, value: Value, depth: usize) anyerror!void {
    switch (value) {
        .string => |s| try writeJsonString(writer, s),
        .object => |obj| {
            if (isCategoricalList(obj)) {
                try writeArrayJson(writer, obj, depth);
            } else {
                try writeObjectJson(writer, obj, depth);
            }
        },
    }
}

/// Returns true if all values in the object are empty objects (categorical list pattern).
fn isCategoricalList(obj: *const Object) bool {
    if (obj.entries.items.len == 0) return false;
    for (obj.entries.items) |entry| {
        switch (entry.value) {
            .object => |child| {
                if (child.entries.items.len != 0) return false;
            },
            .string => return false,
        }
    }
    return true;
}

fn writeArrayJson(writer: anytype, obj: *const Object, depth: usize) !void {
    try writer.writeAll("[\n");
    for (obj.entries.items, 0..) |entry, i| {
        try writeIndent(writer, depth + 1);
        try writeJsonString(writer, entry.key);
        if (i + 1 < obj.entries.items.len) {
            try writer.writeByte(',');
        }
        try writer.writeByte('\n');
    }
    try writeIndent(writer, depth);
    try writer.writeByte(']');
}

fn writeIndent(writer: anytype, depth: usize) !void {
    for (0..depth) |_| {
        try writer.writeAll("  ");
    }
}

fn writeJsonString(writer: anytype, s: []const u8) !void {
    try writer.writeByte('"');
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    try writer.print("\\u{x:0>4}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "simple key-value" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "name = Alice\nage = 30\n");
    try std.testing.expectEqual(@as(usize, 2), root.entries.items.len);
    try std.testing.expectEqualStrings("name", root.entries.items[0].key);
    try std.testing.expectEqualStrings("Alice", root.entries.items[0].value.string);
    try std.testing.expectEqualStrings("age", root.entries.items[1].key);
    try std.testing.expectEqualStrings("30", root.entries.items[1].value.string);
}

test "nested objects" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a,
        \\server =
        \\    host = localhost
        \\    port = 8080
    );
    try std.testing.expectEqual(@as(usize, 1), root.entries.items.len);
    const server = root.entries.items[0].value.object;
    try std.testing.expectEqual(@as(usize, 2), server.entries.items.len);
    try std.testing.expectEqualStrings("host", server.entries.items[0].key);
    try std.testing.expectEqualStrings("localhost", server.entries.items[0].value.string);
    try std.testing.expectEqualStrings("port", server.entries.items[1].key);
    try std.testing.expectEqualStrings("8080", server.entries.items[1].value.string);
}

test "repeated keys become categorical" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a,
        \\tags = web
        \\tags = api
        \\tags = production
    );
    try std.testing.expectEqual(@as(usize, 1), root.entries.items.len);
    const tags = root.entries.items[0].value.object;
    // Should be categorical: {web:{}, api:{}, production:{}}
    try std.testing.expectEqual(@as(usize, 3), tags.entries.items.len);
    try std.testing.expectEqualStrings("web", tags.entries.items[0].key);
    try std.testing.expectEqualStrings("api", tags.entries.items[1].key);
    try std.testing.expectEqualStrings("production", tags.entries.items[2].key);
    try std.testing.expect(isCategoricalList(tags));
}

test "comments become / key" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a,
        \\/= First comment
        \\name = test
        \\/= Second comment
    );
    try std.testing.expectEqual(@as(usize, 2), root.entries.items.len);
    // "/" key should be first (categorical list of comments)
    try std.testing.expectEqualStrings("/", root.entries.items[0].key);
    const comments = root.entries.items[0].value.object;
    try std.testing.expect(isCategoricalList(comments));
    try std.testing.expectEqual(@as(usize, 2), comments.entries.items.len);
    try std.testing.expectEqualStrings("First comment", comments.entries.items[0].key);
    try std.testing.expectEqualStrings("Second comment", comments.entries.items[1].key);
}

test "blank lines are skipped" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a,
        \\name = Alice
        \\
        \\city = NYC
    );
    try std.testing.expectEqual(@as(usize, 2), root.entries.items.len);
    try std.testing.expectEqualStrings("name", root.entries.items[0].key);
    try std.testing.expectEqualStrings("city", root.entries.items[1].key);
}

test "deep nesting" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a,
        \\database =
        \\    connection =
        \\        host = db.example.com
        \\        port = 5432
    );
    const db = root.entries.items[0].value.object;
    const conn = db.entries.items[0].value.object;
    try std.testing.expectEqualStrings("db.example.com", conn.entries.items[0].value.string);
    try std.testing.expectEqualStrings("5432", conn.entries.items[1].value.string);
}

test "header lines are skipped" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a,
        \\#= kvl 1.0
        \\name = test
    );
    try std.testing.expectEqual(@as(usize, 1), root.entries.items.len);
    try std.testing.expectEqualStrings("name", root.entries.items[0].key);
}

test "categorical list detection" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    // Object with all empty children → categorical list
    const cat = try createObject(a);
    try cat.put(a, "a", .{ .object = try createObject(a) });
    try cat.put(a, "b", .{ .object = try createObject(a) });
    try std.testing.expect(isCategoricalList(cat));

    // Object with string children → not categorical
    const obj = try createObject(a);
    try obj.put(a, "x", .{ .string = "1" });
    try std.testing.expect(!isCategoricalList(obj));

    // Empty object → not categorical list
    const empty = try createObject(a);
    try std.testing.expect(!isCategoricalList(empty));
}

test "json output simple" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "name = Alice\ncity = NYC\n");
    var buf: std.ArrayList(u8) = .{};
    defer buf.deinit(alloc);
    try writeCompactedJson(buf.writer(alloc), root);

    const expected =
        \\{
        \\  "name": "Alice",
        \\  "city": "NYC"
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, buf.items);
}

test "json output categorical as array" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a,
        \\tags = web
        \\tags = api
    );
    var buf: std.ArrayList(u8) = .{};
    defer buf.deinit(alloc);
    try writeCompactedJson(buf.writer(alloc), root);

    const expected =
        \\{
        \\  "tags": [
        \\    "web",
        \\    "api"
        \\  ]
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, buf.items);
}

test "escape separator in value" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    // "equation = x\=y" should parse as key="equation", value="x=y"
    // The \= is unescaped after parsing, matching Python/Go/JS behavior
    const root = try parse(a, "equation = x\\=y\n");
    try std.testing.expectEqualStrings("equation", root.entries.items[0].key);
    try std.testing.expectEqualStrings("x=y", root.entries.items[0].value.string);
}

test "escape separator in key" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    // "key\=name = value" should parse as key="key=name", value="value"
    const root = try parse(a, "key\\=name = value\n");
    try std.testing.expectEqualStrings("key=name", root.entries.items[0].key);
    try std.testing.expectEqualStrings("value", root.entries.items[0].value.string);
}

test "no unescape of non-separator backslash" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    // Backslash before non-separator characters is preserved literally
    const root = try parse(a, "path = C:\\Users\\test\n");
    try std.testing.expectEqualStrings("path", root.entries.items[0].key);
    try std.testing.expectEqualStrings("C:\\Users\\test", root.entries.items[0].value.string);
}
