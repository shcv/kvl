const std = @import("std");
const Allocator = std.mem.Allocator;

// ---------------------------------------------------------------------------
// Security limits
// ---------------------------------------------------------------------------

pub const MAX_INPUT_SIZE: usize = 10 * 1024 * 1024; // 10MB
pub const MAX_RECURSION_DEPTH: usize = 100;

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

pub const Config = struct {
    separator: []const u8 = "=",
    version: []const u8 = "1.0",
    space_before: bool = true,
    space_after: bool = true,
    list_markers: []const u8 = "", // Characters like "-+*"

    pub fn autoForSeparator(alloc: Allocator, separator: []const u8) !Config {
        const sep = try alloc.dupe(u8, separator);
        if (sep.len == 1 and sep[0] == ':') {
            return Config{
                .separator = sep,
                .space_before = false,
                .space_after = true,
            };
        }
        return Config{
            .separator = sep,
            .space_before = true,
            .space_after = true,
        };
    }
};

/// Parse a KVL header from the first line.
/// Returns null if no header found.
pub fn parseHeader(alloc: Allocator, text: []const u8) !?Config {
    // Find first line
    const first_line_end = std.mem.indexOfScalar(u8, text, '\n') orelse text.len;
    var first_line = text[0..first_line_end];
    // Trim trailing \r
    if (first_line.len > 0 and first_line[first_line.len - 1] == '\r') {
        first_line = first_line[0 .. first_line.len - 1];
    }
    // Trim whitespace
    first_line = std.mem.trim(u8, first_line, " \t");

    if (first_line.len == 0 or first_line[0] != '#') return null;

    // Find " kvl " in the line
    const kvl_pos = std.mem.indexOf(u8, first_line, " kvl") orelse return null;
    // After " kvl" should be end of line or space
    const after_kvl = kvl_pos + 4;
    if (after_kvl < first_line.len and first_line[after_kvl] != ' ') return null;

    const separator = try alloc.dupe(u8, first_line[1..kvl_pos]);

    // Parse rest after " kvl "
    var rest = if (after_kvl < first_line.len) first_line[after_kvl + 1 ..] else "";
    rest = std.mem.trim(u8, rest, " \t");

    // Parse version (first token)
    var version: []const u8 = "1.0";
    var list_markers: []const u8 = "";

    if (rest.len > 0) {
        // Split by spaces
        var parts = std.ArrayListUnmanaged([]const u8){};
        defer parts.deinit(alloc);

        var iter = std.mem.tokenizeAny(u8, rest, " \t");
        while (iter.next()) |part| {
            try parts.append(alloc, part);
        }

        if (parts.items.len > 0) {
            version = try alloc.dupe(u8, parts.items[0]);
        }

        // Check if second part is list markers (non-alphanumeric, no '=')
        if (parts.items.len > 1) {
            const second = parts.items[1];
            if (std.mem.indexOfScalar(u8, second, '=') == null and !isAlphanumeric(second)) {
                list_markers = try alloc.dupe(u8, second);
            }
        }
    } else {
        version = try alloc.dupe(u8, "1.0");
    }

    // Auto-configure spacing based on separator
    var config = try Config.autoForSeparator(alloc, separator);
    // Free the dupe from autoForSeparator since we already have separator
    alloc.free(config.separator);
    config.separator = separator;
    config.version = version;
    config.list_markers = list_markers;

    return config;
}

fn isAlphanumeric(s: []const u8) bool {
    for (s) |c| {
        if (!std.ascii.isAlphanumeric(c)) return false;
    }
    return s.len > 0;
}

/// Extract content from KVL text, removing header if present.
fn extractContent(text: []const u8) []const u8 {
    // Find first line
    const first_line_end = std.mem.indexOfScalar(u8, text, '\n') orelse return text;
    var first_line = text[0..first_line_end];
    if (first_line.len > 0 and first_line[first_line.len - 1] == '\r') {
        first_line = first_line[0 .. first_line.len - 1];
    }
    const trimmed = std.mem.trim(u8, first_line, " \t");
    if (trimmed.len > 0 and trimmed[0] == '#' and std.mem.indexOf(u8, trimmed, " kvl ") != null) {
        // Remove header line
        return text[first_line_end + 1 ..];
    }
    return text;
}

// ---------------------------------------------------------------------------
// Data model - JSON-compatible value types
// ---------------------------------------------------------------------------

pub const Value = union(enum) {
    object: *OrderedMap,
    array: *std.ArrayListUnmanaged(Value),
    string: []const u8,

    pub fn eql(a: Value, b: Value) bool {
        switch (a) {
            .string => |sa| switch (b) {
                .string => |sb| return std.mem.eql(u8, sa, sb),
                else => return false,
            },
            .object => |oa| switch (b) {
                .object => |ob| {
                    if (oa.entries.items.len != ob.entries.items.len) return false;
                    for (oa.entries.items, ob.entries.items) |ea, eb| {
                        if (!std.mem.eql(u8, ea.key, eb.key)) return false;
                        if (!ea.value.eql(eb.value)) return false;
                    }
                    return true;
                },
                else => return false,
            },
            .array => |aa| switch (b) {
                .array => |ab| {
                    if (aa.items.len != ab.items.len) return false;
                    for (aa.items, ab.items) |va, vb| {
                        if (!va.eql(vb)) return false;
                    }
                    return true;
                },
                else => return false,
            },
        }
    }
};

pub const OrderedMap = struct {
    entries: std.ArrayListUnmanaged(Entry) = .{},

    pub const Entry = struct {
        key: []const u8,
        value: Value,
    };

    pub fn findIndex(self: *const OrderedMap, key: []const u8) ?usize {
        for (self.entries.items, 0..) |entry, i| {
            if (std.mem.eql(u8, entry.key, key)) return i;
        }
        return null;
    }

    pub fn get(self: *const OrderedMap, key: []const u8) ?Value {
        if (self.findIndex(key)) |i| return self.entries.items[i].value;
        return null;
    }

    pub fn put(self: *OrderedMap, alloc: Allocator, key: []const u8, value: Value) !void {
        try self.entries.append(alloc, .{ .key = key, .value = value });
    }

    pub fn putOrReplace(self: *OrderedMap, alloc: Allocator, key: []const u8, value: Value) !void {
        if (self.findIndex(key)) |i| {
            self.entries.items[i].value = value;
        } else {
            try self.entries.append(alloc, .{ .key = key, .value = value });
        }
    }

    /// Merge a value under `key` using categorical merge semantics.
    pub fn mergeKey(self: *OrderedMap, alloc: Allocator, key: []const u8, value: Value) Allocator.Error!void {
        const idx = self.findIndex(key) orelse {
            try self.put(alloc, key, value);
            return;
        };
        self.entries.items[idx].value = try mergeValues(alloc, self.entries.items[idx].value, value);
    }
};

fn createMap(alloc: Allocator) !*OrderedMap {
    const obj = try alloc.create(OrderedMap);
    obj.* = .{};
    return obj;
}

fn createArray(alloc: Allocator) !*std.ArrayListUnmanaged(Value) {
    const arr = try alloc.create(std.ArrayListUnmanaged(Value));
    arr.* = .{};
    return arr;
}

// ---------------------------------------------------------------------------
// Merge operation
// ---------------------------------------------------------------------------

/// Merge two categorical values recursively.
pub fn mergeValues(alloc: Allocator, a: Value, b: Value) Allocator.Error!Value {
    switch (a) {
        .object => |oa| switch (b) {
            .object => |ob| {
                // object + object -> recursive merge
                const result = try createMap(alloc);
                for (oa.entries.items) |entry| {
                    try result.put(alloc, entry.key, entry.value);
                }
                for (ob.entries.items) |entry| {
                    if (result.findIndex(entry.key)) |idx| {
                        result.entries.items[idx].value = try mergeValues(alloc, result.entries.items[idx].value, entry.value);
                    } else {
                        try result.put(alloc, entry.key, entry.value);
                    }
                }
                return .{ .object = result };
            },
            .string => |sb| {
                // object + string: add string as categorical entry
                const result = try createMap(alloc);
                for (oa.entries.items) |entry| {
                    try result.put(alloc, entry.key, entry.value);
                }
                if (sb.len > 0) {
                    try result.mergeKey(alloc, sb, .{ .object = try createMap(alloc) });
                }
                return .{ .object = result };
            },
            .array => |_| {
                return a;
            },
        },
        .string => |sa| switch (b) {
            .object => |ob| {
                // string + object: wrap string, merge object entries
                const result = try createMap(alloc);
                if (sa.len > 0) {
                    try result.put(alloc, sa, .{ .object = try createMap(alloc) });
                }
                for (ob.entries.items) |entry| {
                    try result.mergeKey(alloc, entry.key, entry.value);
                }
                return .{ .object = result };
            },
            .string => |sb| {
                // string + string -> categorical {old:{}, new:{}}
                const cat = try createMap(alloc);
                if (sa.len > 0) {
                    try cat.put(alloc, sa, .{ .object = try createMap(alloc) });
                }
                if (sb.len > 0) {
                    try cat.put(alloc, sb, .{ .object = try createMap(alloc) });
                }
                return .{ .object = cat };
            },
            .array => |_| {
                return b;
            },
        },
        .array => |_| switch (b) {
            .object => |_| return b,
            .string => |_| return a,
            .array => |_| return a,
        },
    }
}

/// Merge two OrderedMap objects.
pub fn mergeMaps(alloc: Allocator, a: *const OrderedMap, b: *const OrderedMap) !*OrderedMap {
    const result = try createMap(alloc);
    for (a.entries.items) |entry| {
        try result.put(alloc, entry.key, entry.value);
    }
    for (b.entries.items) |entry| {
        try result.mergeKey(alloc, entry.key, entry.value);
    }
    return result;
}

// ---------------------------------------------------------------------------
// Compact / Expand transforms
// ---------------------------------------------------------------------------

/// Check if an object is a categorical list (all values are empty objects).
fn isCategoricalList(obj: *const OrderedMap) bool {
    if (obj.entries.items.len == 0) return false;
    for (obj.entries.items) |entry| {
        switch (entry.value) {
            .object => |child| {
                if (child.entries.items.len != 0) return false;
            },
            else => return false,
        }
    }
    return true;
}

/// Check if a string looks like a section header (starts with the separator).
fn isSectionHeader(value: []const u8, separator: []const u8) bool {
    const trimmed_val = std.mem.trim(u8, value, " \t");
    if (trimmed_val.len == 0) return false;
    return std.mem.startsWith(u8, trimmed_val, separator);
}

/// Compact a categorical Value into user-friendly form.
/// - Single empty child {val: {}} -> "val"
/// - All empty children {a:{}, b:{}, c:{}} -> ["a", "b", "c"]
/// - Singleton empty key {"": content} -> content
pub fn compact(alloc: Allocator, val: Value, config: Config) !Value {
    switch (val) {
        .string => return val,
        .array => |arr| {
            const new_arr = try createArray(alloc);
            for (arr.items) |item| {
                try new_arr.append(alloc, try compact(alloc, item, config));
            }
            return .{ .array = new_arr };
        },
        .object => |obj| {
            if (obj.entries.items.len == 0) return val;

            // First recursively compact children
            const compacted = try createMap(alloc);
            for (obj.entries.items) |entry| {
                try compacted.put(alloc, entry.key, try compact(alloc, entry.value, config));
            }

            // Rule 1: Singleton empty key lifting: {"": content} -> content
            if (compacted.entries.items.len == 1 and compacted.entries.items[0].key.len == 0) {
                const empty_value = compacted.entries.items[0].value;
                switch (empty_value) {
                    .object => |ev| {
                        if (ev.entries.items.len > 0) {
                            // Check if all values are objects
                            var all_objects = true;
                            for (ev.entries.items) |e| {
                                switch (e.value) {
                                    .object => {},
                                    else => {
                                        all_objects = false;
                                        break;
                                    },
                                }
                            }
                            if (all_objects) {
                                // Convert to array of single-key objects
                                const result_arr = try createArray(alloc);
                                for (ev.entries.items) |e| {
                                    const item_obj = try createMap(alloc);
                                    try item_obj.put(alloc, e.key, e.value);
                                    try result_arr.append(alloc, .{ .object = item_obj });
                                }
                                return .{ .array = result_arr };
                            }
                        }
                        return empty_value;
                    },
                    else => return empty_value,
                }
            }

            // Rule 2: Single empty child -> string
            if (compacted.entries.items.len == 1) {
                const child_key = compacted.entries.items[0].key;
                const child_value = compacted.entries.items[0].value;
                if (child_key.len > 0) {
                    switch (child_value) {
                        .object => |cv| {
                            if (cv.entries.items.len == 0) {
                                return .{ .string = child_key };
                            }
                        },
                        else => {},
                    }
                }
            }

            // Rule 3: All empty children -> list
            var all_empty = true;
            for (compacted.entries.items) |entry| {
                switch (entry.value) {
                    .object => |cv| {
                        if (cv.entries.items.len != 0) {
                            all_empty = false;
                            break;
                        }
                    },
                    else => {
                        all_empty = false;
                        break;
                    },
                }
            }
            if (all_empty) {
                // Check for section headers
                for (compacted.entries.items) |entry| {
                    if (isSectionHeader(entry.key, config.separator)) {
                        return .{ .object = compacted };
                    }
                }
                // Flatten to list
                const result_arr = try createArray(alloc);
                for (compacted.entries.items) |entry| {
                    try result_arr.append(alloc, .{ .string = entry.key });
                }
                return .{ .array = result_arr };
            }

            return .{ .object = compacted };
        },
    }
}

/// Expand a compacted Value back to categorical form.
pub fn expand(alloc: Allocator, val: Value) !Value {
    switch (val) {
        .string => |s| {
            // "value" -> {"value": {}}
            const obj = try createMap(alloc);
            try obj.put(alloc, s, .{ .object = try createMap(alloc) });
            return .{ .object = obj };
        },
        .array => |arr| {
            // ["a", "b"] -> {"a": {}, "b": {}}
            const obj = try createMap(alloc);
            for (arr.items) |item| {
                switch (item) {
                    .string => |s| {
                        try obj.put(alloc, s, .{ .object = try createMap(alloc) });
                    },
                    else => {
                        const expanded = try expand(alloc, item);
                        // Merge into empty key
                        if (obj.findIndex("")) |idx| {
                            obj.entries.items[idx].value = try mergeValues(alloc, obj.entries.items[idx].value, expanded);
                        } else {
                            try obj.put(alloc, "", expanded);
                        }
                    },
                }
            }
            return .{ .object = obj };
        },
        .object => |obj| {
            const result = try createMap(alloc);
            for (obj.entries.items) |entry| {
                try result.put(alloc, entry.key, try expand(alloc, entry.value));
            }
            return .{ .object = result };
        },
    }
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

pub const ParseError = error{
    InputTooLarge,
    MaxDepthExceeded,
    MixedIndentation,
    OutOfMemory,
};

const IndentMode = enum {
    unknown,
    spaces,
    tabs,
};

const StackEntry = struct {
    indent: i32,
    obj: *OrderedMap,
};

/// Parse KVL text into a categorical object tree (low-level API).
/// Returns the raw categorical model where repeated keys create nested dicts.
pub fn parse(alloc: Allocator, input: []const u8) ParseError!*OrderedMap {
    return parseWithConfig(alloc, input, null);
}

/// Parse KVL text into categorical object tree with explicit config.
pub fn parseWithConfig(alloc: Allocator, input: []const u8, config_opt: ?Config) ParseError!*OrderedMap {
    if (input.len > MAX_INPUT_SIZE) {
        return ParseError.InputTooLarge;
    }

    // Determine config: use provided, or parse header, or use default
    const config = config_opt orelse (parseHeader(alloc, input) catch null) orelse Config{};
    const separator = config.separator;
    const list_markers = config.list_markers;

    // Extract content (skip header line if present)
    const content = extractContent(input);

    const root = try createMap(alloc);
    var stack = std.ArrayListUnmanaged(StackEntry){};
    defer stack.deinit(alloc);
    try stack.append(alloc, .{ .indent = -1, .obj = root });

    var indent_mode: IndentMode = .unknown;
    var current_list_key: ?[]const u8 = null;
    var list_parent: ?*OrderedMap = null;

    // Split into lines
    var lines_list = std.ArrayListUnmanaged([]const u8){};
    defer lines_list.deinit(alloc);
    {
        var line_iter = std.mem.splitSequence(u8, content, "\n");
        while (line_iter.next()) |raw_line| {
            // Strip trailing \r
            const line = if (raw_line.len > 0 and raw_line[raw_line.len - 1] == '\r')
                raw_line[0 .. raw_line.len - 1]
            else
                raw_line;
            try lines_list.append(alloc, line);
        }
    }
    const lines = lines_list.items;

    var i: usize = 0;
    while (i < lines.len) {
        const line = lines[i];

        // Skip blank lines
        if (isBlank(line)) {
            i += 1;
            continue;
        }

        const raw_indent = measureRawIndent(line);
        const indent = computeIndent(line, raw_indent);

        // Check/enforce indent mode
        if (raw_indent > 0) {
            const first_ws = line[0];
            if (first_ws == ' ') {
                if (indent_mode == .tabs) return ParseError.MixedIndentation;
                indent_mode = .spaces;
            } else if (first_ws == '\t') {
                if (indent_mode == .spaces) return ParseError.MixedIndentation;
                indent_mode = .tabs;
            }
        }

        const content_start: usize = raw_indent;
        const line_content = line[content_start..];

        // Check for list marker
        if (list_markers.len > 0 and isListMarker(line_content, list_markers)) {
            const item_content = std.mem.trim(u8, line_content[2..], " \t");

            if (current_list_key) |list_key| {
                if (list_parent) |lp| {
                    // List items create an array of single-key objects
                    if (lp.findIndex(list_key)) |idx| {
                        const existing = &lp.entries.items[idx].value;
                        switch (existing.*) {
                            .array => |arr| {
                                // Append to existing array
                                const item_obj = try createMap(alloc);
                                try item_obj.put(alloc, item_content, .{ .object = try createMap(alloc) });
                                try arr.append(alloc, .{ .object = item_obj });
                            },
                            .object => |obj| {
                                if (obj.entries.items.len == 0) {
                                    // Convert empty object to array
                                    const arr = try createArray(alloc);
                                    const item_obj = try createMap(alloc);
                                    try item_obj.put(alloc, item_content, .{ .object = try createMap(alloc) });
                                    try arr.append(alloc, .{ .object = item_obj });
                                    existing.* = .{ .array = arr };
                                } else {
                                    // Merge into existing object as categorical entry
                                    try obj.mergeKey(alloc, item_content, .{ .object = try createMap(alloc) });
                                }
                            },
                            else => {},
                        }
                    }
                }
            }
            i += 1;
            continue;
        }

        // Pop stack to find correct parent
        while (stack.items.len > 1 and stack.items[stack.items.len - 1].indent >= indent) {
            _ = stack.pop();
        }
        const parent = stack.items[stack.items.len - 1].obj;

        // Find the separator
        const sep_pos = findUnescapedSeparator(line_content, separator);

        if (sep_pos == null) {
            // No separator found
            if (raw_indent > 0 and parent.entries.items.len > 0) {
                // Indented line without separator = multiline continuation
                const last_idx = parent.entries.items.len - 1;
                const existing = &parent.entries.items[last_idx].value;
                switch (existing.*) {
                    .string => |old_str| {
                        const new_str = try std.fmt.allocPrint(alloc, "{s}\n{s}", .{ old_str, std.mem.trimRight(u8, line, " \t\r") });
                        existing.* = .{ .string = new_str };
                    },
                    .object => |_| {},
                    .array => |_| {},
                }
            }
            i += 1;
            continue;
        }

        const raw_key = line_content[0..sep_pos.?];
        const raw_value = line_content[sep_pos.? + separator.len ..];

        const key = std.mem.trim(u8, raw_key, " \t");
        const val = std.mem.trim(u8, raw_value, " \t");

        if (val.len == 0) {
            // Empty value: check for multiline continuation or nested object
            if (i + 1 < lines.len) {
                const next_line = lines[i + 1];
                if (!isBlank(next_line)) {
                    const next_raw_indent = measureRawIndent(next_line);
                    const next_indent = computeIndent(next_line, next_raw_indent);
                    if (next_indent > indent) {
                        const next_content = next_line[next_raw_indent..];
                        const next_has_sep = findUnescapedSeparator(next_content, separator) != null;
                        const next_is_list = list_markers.len > 0 and isListMarker(next_content, list_markers);

                        if (next_has_sep or next_is_list) {
                            // Nested structure follows
                            const child = try getOrCreateNested(parent, alloc, key);
                            try stack.append(alloc, .{ .indent = indent, .obj = child });
                            current_list_key = key;
                            list_parent = parent;
                            i += 1;
                            continue;
                        } else {
                            // Multiline value continuation
                            const multiline_val = try collectMultilineValue(alloc, lines, i + 1, indent);
                            try parent.mergeKey(alloc, key, .{ .string = multiline_val });
                            // Skip consumed lines
                            var skip: usize = i + 1;
                            while (skip < lines.len) {
                                const ml = lines[skip];
                                if (isBlank(ml)) {
                                    skip += 1;
                                    continue;
                                }
                                const ml_indent = computeIndent(ml, measureRawIndent(ml));
                                if (ml_indent <= indent) break;
                                skip += 1;
                            }
                            i = skip;
                            current_list_key = null;
                            list_parent = null;
                            continue;
                        }
                    }
                }
            }
            // No nested content: empty object
            const child = try getOrCreateNested(parent, alloc, key);
            try stack.append(alloc, .{ .indent = indent, .obj = child });
            current_list_key = key;
            list_parent = parent;
        } else {
            // Simple key-value
            try parent.mergeKey(alloc, key, .{ .string = val });
            current_list_key = null;
            list_parent = null;
        }
        i += 1;
    }

    // Unescape separator patterns in all keys and string values
    try unescapeTree(alloc, root, separator);

    return root;
}

/// High-level parse: returns compacted JSON-like structure.
pub fn loads(alloc: Allocator, input: []const u8) !Value {
    return loadsWithConfig(alloc, input, null);
}

pub fn loadsWithConfig(alloc: Allocator, input: []const u8, config_opt: ?Config) !Value {
    const config = config_opt orelse (parseHeader(alloc, input) catch null) orelse Config{};
    const root = try parseWithConfig(alloc, input, config);
    return compact(alloc, .{ .object = root }, config);
}

fn isBlank(line: []const u8) bool {
    for (line) |c| {
        if (c != ' ' and c != '\t') return false;
    }
    return true;
}

fn isListMarker(content_line: []const u8, markers: []const u8) bool {
    if (content_line.len < 2) return false;
    for (markers) |m| {
        if (content_line[0] == m and (content_line[1] == ' ' or content_line[1] == '\t')) {
            return true;
        }
    }
    return false;
}

/// Count raw indentation characters (not normalized).
fn measureRawIndent(line: []const u8) usize {
    var n: usize = 0;
    for (line) |c| {
        if (c == ' ' or c == '\t') {
            n += 1;
        } else {
            break;
        }
    }
    return n;
}

/// Compute normalized indent level. Tabs count as 4 spaces.
fn computeIndent(line: []const u8, raw: usize) i32 {
    var n: i32 = 0;
    for (line[0..raw]) |c| {
        if (c == '\t') {
            n += 4;
        } else {
            n += 1;
        }
    }
    return n;
}

/// Find the first unescaped occurrence of separator in line.
fn findUnescapedSeparator(line: []const u8, separator: []const u8) ?usize {
    if (separator.len == 0) return null;
    var pos: usize = 0;
    while (pos < line.len) {
        if (pos + separator.len <= line.len and std.mem.eql(u8, line[pos .. pos + separator.len], separator)) {
            // Check if preceded by backslash
            if (pos > 0 and line[pos - 1] == '\\') {
                pos += 1;
                continue;
            }
            return pos;
        }
        pos += 1;
    }
    return null;
}

/// Collect multi-line value starting from `start_idx`, where content
/// must be indented more than `parent_indent`.
fn collectMultilineValue(alloc: Allocator, lines: []const []const u8, start_idx: usize, parent_indent: i32) ![]const u8 {
    var value_lines = std.ArrayListUnmanaged([]const u8){};
    defer value_lines.deinit(alloc);

    var idx = start_idx;
    while (idx < lines.len) {
        const line = lines[idx];
        if (isBlank(line)) {
            try value_lines.append(alloc, line);
            idx += 1;
            continue;
        }
        const line_indent = computeIndent(line, measureRawIndent(line));
        if (line_indent <= parent_indent) break;
        try value_lines.append(alloc, std.mem.trimRight(u8, line, " \t\r"));
        idx += 1;
    }

    // Build the multiline string with leading newline (matching Python behavior)
    if (value_lines.items.len == 0) return "";

    var result = std.ArrayListUnmanaged(u8){};
    try result.append(alloc, '\n');
    for (value_lines.items, 0..) |vl, j| {
        try result.appendSlice(alloc, vl);
        if (j + 1 < value_lines.items.len) {
            try result.append(alloc, '\n');
        }
    }
    return result.items;
}

/// Get an existing object for `key`, or create a new one.
fn getOrCreateNested(parent: *OrderedMap, alloc: Allocator, key: []const u8) !*OrderedMap {
    if (parent.findIndex(key)) |idx| {
        const existing = &parent.entries.items[idx].value;
        switch (existing.*) {
            .object => |obj| return obj,
            .string => |s| {
                const cat = try createMap(alloc);
                if (s.len > 0) {
                    try cat.put(alloc, s, .{ .object = try createMap(alloc) });
                }
                existing.* = .{ .object = cat };
                return cat;
            },
            .array => |_| {
                const obj = try createMap(alloc);
                existing.* = .{ .object = obj };
                return obj;
            },
        }
    }
    const obj = try createMap(alloc);
    try parent.put(alloc, key, .{ .object = obj });
    return obj;
}

// ---------------------------------------------------------------------------
// Escape sequence processing
// ---------------------------------------------------------------------------

/// Unescape separator patterns: \<sep> -> <sep>
fn unescapeText(alloc: Allocator, text: []const u8, separator: []const u8) ![]const u8 {
    if (separator.len == 0) return text;

    // Quick check: does text contain \<sep>?
    var has_escape = false;
    var i: usize = 0;
    while (i < text.len) {
        if (text[i] == '\\' and i + separator.len < text.len and
            std.mem.eql(u8, text[i + 1 .. i + 1 + separator.len], separator))
        {
            has_escape = true;
            break;
        }
        i += 1;
    }
    if (!has_escape) return text;

    // Build unescaped result
    var result = std.ArrayListUnmanaged(u8){};
    i = 0;
    while (i < text.len) {
        if (text[i] == '\\' and i + separator.len < text.len and
            std.mem.eql(u8, text[i + 1 .. i + 1 + separator.len], separator))
        {
            // \<sep> -> <sep>
            try result.appendSlice(alloc, separator);
            i += 1 + separator.len;
        } else {
            try result.append(alloc, text[i]);
            i += 1;
        }
    }
    return result.items;
}

/// Recursively unescape all keys and string values.
fn unescapeTree(alloc: Allocator, obj: *OrderedMap, separator: []const u8) !void {
    for (obj.entries.items) |*entry| {
        entry.key = try unescapeText(alloc, entry.key, separator);
        switch (entry.value) {
            .string => |s| {
                entry.value = .{ .string = try unescapeText(alloc, s, separator) };
            },
            .object => |child| {
                try unescapeTree(alloc, child, separator);
            },
            .array => |arr| {
                for (arr.items) |*item| {
                    switch (item.*) {
                        .string => |s| {
                            item.* = .{ .string = try unescapeText(alloc, s, separator) };
                        },
                        .object => |child| {
                            try unescapeTree(alloc, child, separator);
                        },
                        .array => |_| {},
                    }
                }
            },
        }
    }
}

// ---------------------------------------------------------------------------
// Serializer: Value -> KVL text
// ---------------------------------------------------------------------------

pub fn serialize(alloc: Allocator, val: Value, config_opt: ?Config) ![]const u8 {
    const config = config_opt orelse Config{};
    var buf = std.ArrayListUnmanaged(u8){};
    try serializeValue(alloc, &buf, val, config, 0);
    return buf.items;
}

fn serializeValue(alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), val: Value, config: Config, level: usize) !void {
    switch (val) {
        .object => |obj| {
            try serializeObject(alloc, buf, obj, config, level);
        },
        .array => |arr| {
            // Arrays need to be expanded to categorical format for serialization
            for (arr.items) |item| {
                switch (item) {
                    .string => |s| {
                        try writeIndent(alloc, buf, level, "  ");
                        try writeEscaped(alloc, buf, s, config.separator);
                        try writeSep(alloc, buf, config, true);
                        try buf.append(alloc, '\n');
                    },
                    .object => |child_obj| {
                        try serializeObject(alloc, buf, child_obj, config, level);
                    },
                    .array => |_| {},
                }
            }
        },
        .string => |s| {
            try writeEscaped(alloc, buf, s, config.separator);
        },
    }
}

fn serializeObject(alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), obj: *const OrderedMap, config: Config, level: usize) !void {
    for (obj.entries.items) |entry| {
        try writeIndent(alloc, buf, level, "  ");
        try writeEscaped(alloc, buf, entry.key, config.separator);

        switch (entry.value) {
            .object => |child| {
                try writeSep(alloc, buf, config, true);
                try buf.append(alloc, '\n');
                if (child.entries.items.len > 0) {
                    try serializeObject(alloc, buf, child, config, level + 1);
                }
            },
            .array => |arr| {
                // Expand array back to categorical
                try writeSep(alloc, buf, config, true);
                try buf.append(alloc, '\n');
                for (arr.items) |item| {
                    switch (item) {
                        .string => |s| {
                            try writeIndent(alloc, buf, level + 1, "  ");
                            try writeEscaped(alloc, buf, s, config.separator);
                            try writeSep(alloc, buf, config, true);
                            try buf.append(alloc, '\n');
                        },
                        .object => |child_obj| {
                            try serializeObject(alloc, buf, child_obj, config, level + 1);
                        },
                        .array => |_| {},
                    }
                }
            },
            .string => |s| {
                if (s.len == 0) {
                    try writeSep(alloc, buf, config, true);
                    try buf.append(alloc, '\n');
                } else if (std.mem.indexOfScalar(u8, s, '\n') != null) {
                    try writeSep(alloc, buf, config, true);
                    try writeEscaped(alloc, buf, s, config.separator);
                    try buf.append(alloc, '\n');
                } else {
                    try writeSep(alloc, buf, config, false);
                    try writeEscaped(alloc, buf, s, config.separator);
                    try buf.append(alloc, '\n');
                }
            },
        }
    }
}

fn writeSep(alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), config: Config, for_empty: bool) !void {
    if (config.space_before) try buf.append(alloc, ' ');
    try buf.appendSlice(alloc, config.separator);
    if (!for_empty and config.space_after) try buf.append(alloc, ' ');
}

fn writeIndent(alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), level: usize, indent: []const u8) !void {
    for (0..level) |_| {
        try buf.appendSlice(alloc, indent);
    }
}

fn writeEscaped(alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), text: []const u8, separator: []const u8) !void {
    if (separator.len == 0) {
        try buf.appendSlice(alloc, text);
        return;
    }
    var i: usize = 0;
    while (i < text.len) {
        if (i + separator.len <= text.len and std.mem.eql(u8, text[i .. i + separator.len], separator)) {
            try buf.append(alloc, '\\');
            try buf.appendSlice(alloc, separator);
            i += separator.len;
        } else {
            try buf.append(alloc, text[i]);
            i += 1;
        }
    }
}

// ---------------------------------------------------------------------------
// JSON output (for CLI)
// ---------------------------------------------------------------------------

pub fn writeJson(writer: anytype, val: Value, depth: usize) anyerror!void {
    switch (val) {
        .string => |s| try writeJsonString(writer, s),
        .object => |obj| try writeObjectJson(writer, obj, depth),
        .array => |arr| try writeArrayJson(writer, arr, depth),
    }
}

pub fn writeJsonRoot(writer: anytype, val: Value) !void {
    try writeJson(writer, val, 0);
    try writer.writeByte('\n');
}

fn writeObjectJson(writer: anytype, obj: *const OrderedMap, depth: usize) anyerror!void {
    if (obj.entries.items.len == 0) {
        try writer.writeAll("{}");
        return;
    }
    try writer.writeAll("{\n");
    for (obj.entries.items, 0..) |entry, idx| {
        try writeJsonIndent(writer, depth + 1);
        try writeJsonString(writer, entry.key);
        try writer.writeAll(": ");
        try writeJson(writer, entry.value, depth + 1);
        if (idx + 1 < obj.entries.items.len) {
            try writer.writeByte(',');
        }
        try writer.writeByte('\n');
    }
    try writeJsonIndent(writer, depth);
    try writer.writeByte('}');
}

fn writeArrayJson(writer: anytype, arr: *const std.ArrayListUnmanaged(Value), depth: usize) anyerror!void {
    if (arr.items.len == 0) {
        try writer.writeAll("[]");
        return;
    }
    try writer.writeAll("[\n");
    for (arr.items, 0..) |item, idx| {
        try writeJsonIndent(writer, depth + 1);
        try writeJson(writer, item, depth + 1);
        if (idx + 1 < arr.items.len) {
            try writer.writeByte(',');
        }
        try writer.writeByte('\n');
    }
    try writeJsonIndent(writer, depth);
    try writer.writeByte(']');
}

fn writeJsonIndent(writer: anytype, depth: usize) !void {
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

/// Write categorical object as JSON (raw parse output).
pub fn writeCategoricalJson(writer: anytype, obj: *const OrderedMap) !void {
    try writeCategoricalObjectJson(writer, obj, 0);
    try writer.writeByte('\n');
}

fn writeCategoricalValueJson(writer: anytype, val: Value, depth: usize) anyerror!void {
    switch (val) {
        .string => |s| {
            // In categorical model, string values are wrapped: "foo" -> {"foo": {}}
            try writer.writeAll("{\n");
            try writeJsonIndent(writer, depth + 1);
            try writeJsonString(writer, s);
            try writer.writeAll(": {}");
            try writer.writeByte('\n');
            try writeJsonIndent(writer, depth);
            try writer.writeByte('}');
        },
        .object => |obj| try writeCategoricalObjectJson(writer, obj, depth),
        .array => |arr| try writeCategoricalArrayJson(writer, arr, depth),
    }
}

fn writeCategoricalObjectJson(writer: anytype, obj: *const OrderedMap, depth: usize) anyerror!void {
    if (obj.entries.items.len == 0) {
        try writer.writeAll("{}");
        return;
    }
    try writer.writeAll("{\n");
    for (obj.entries.items, 0..) |entry, idx| {
        try writeJsonIndent(writer, depth + 1);
        try writeJsonString(writer, entry.key);
        try writer.writeAll(": ");
        try writeCategoricalValueJson(writer, entry.value, depth + 1);
        if (idx + 1 < obj.entries.items.len) {
            try writer.writeByte(',');
        }
        try writer.writeByte('\n');
    }
    try writeJsonIndent(writer, depth);
    try writer.writeByte('}');
}

fn writeCategoricalArrayJson(writer: anytype, arr: *const std.ArrayListUnmanaged(Value), depth: usize) anyerror!void {
    if (arr.items.len == 0) {
        try writer.writeAll("[]");
        return;
    }
    try writer.writeAll("[\n");
    for (arr.items, 0..) |item, idx| {
        try writeJsonIndent(writer, depth + 1);
        try writeCategoricalValueJson(writer, item, depth + 1);
        if (idx + 1 < arr.items.len) {
            try writer.writeByte(',');
        }
        try writer.writeByte('\n');
    }
    try writeJsonIndent(writer, depth);
    try writer.writeByte(']');
}

// ---------------------------------------------------------------------------
// JSON input parser (for serialize command: read JSON from stdin)
// ---------------------------------------------------------------------------

pub fn parseJson(alloc: Allocator, input: []const u8) Allocator.Error!Value {
    var pos: usize = 0;
    return parseJsonValue(alloc, input, &pos);
}

fn parseJsonValue(alloc: Allocator, input: []const u8, pos: *usize) Allocator.Error!Value {
    skipJsonWhitespace(input, pos);
    if (pos.* >= input.len) return error.OutOfMemory;

    const c = input[pos.*];
    if (c == '"') {
        return .{ .string = try parseJsonStringLiteral(alloc, input, pos) };
    } else if (c == '{') {
        return .{ .object = try parseJsonObject(alloc, input, pos) };
    } else if (c == '[') {
        return .{ .array = try parseJsonArrayValue(alloc, input, pos) };
    } else if (c == 't' or c == 'f') {
        // boolean -> string
        if (std.mem.startsWith(u8, input[pos.*..], "true")) {
            pos.* += 4;
            return .{ .string = try alloc.dupe(u8, "true") };
        } else if (std.mem.startsWith(u8, input[pos.*..], "false")) {
            pos.* += 5;
            return .{ .string = try alloc.dupe(u8, "false") };
        }
        return error.OutOfMemory;
    } else if (c == 'n' and std.mem.startsWith(u8, input[pos.*..], "null")) {
        pos.* += 4;
        return .{ .string = try alloc.dupe(u8, "") };
    } else if (c == '-' or std.ascii.isDigit(c)) {
        // number -> string
        const start = pos.*;
        if (c == '-') pos.* += 1;
        while (pos.* < input.len and (std.ascii.isDigit(input[pos.*]) or input[pos.*] == '.' or input[pos.*] == 'e' or input[pos.*] == 'E' or input[pos.*] == '+' or input[pos.*] == '-')) {
            pos.* += 1;
        }
        return .{ .string = try alloc.dupe(u8, input[start..pos.*]) };
    }
    return error.OutOfMemory;
}

fn parseJsonObject(alloc: Allocator, input: []const u8, pos: *usize) Allocator.Error!*OrderedMap {
    pos.* += 1; // skip '{'
    skipJsonWhitespace(input, pos);

    const obj = try createMap(alloc);

    if (pos.* < input.len and input[pos.*] == '}') {
        pos.* += 1;
        return obj;
    }

    while (true) {
        skipJsonWhitespace(input, pos);
        const key = try parseJsonStringLiteral(alloc, input, pos);
        skipJsonWhitespace(input, pos);
        if (pos.* < input.len and input[pos.*] == ':') pos.* += 1;
        const value = try parseJsonValue(alloc, input, pos);
        try obj.put(alloc, key, value);
        skipJsonWhitespace(input, pos);
        if (pos.* < input.len and input[pos.*] == ',') {
            pos.* += 1;
        } else {
            break;
        }
    }
    skipJsonWhitespace(input, pos);
    if (pos.* < input.len and input[pos.*] == '}') pos.* += 1;
    return obj;
}

fn parseJsonArrayValue(alloc: Allocator, input: []const u8, pos: *usize) Allocator.Error!*std.ArrayListUnmanaged(Value) {
    pos.* += 1; // skip '['
    skipJsonWhitespace(input, pos);

    const arr = try createArray(alloc);

    if (pos.* < input.len and input[pos.*] == ']') {
        pos.* += 1;
        return arr;
    }

    while (true) {
        const value = try parseJsonValue(alloc, input, pos);
        try arr.append(alloc, value);
        skipJsonWhitespace(input, pos);
        if (pos.* < input.len and input[pos.*] == ',') {
            pos.* += 1;
        } else {
            break;
        }
    }
    skipJsonWhitespace(input, pos);
    if (pos.* < input.len and input[pos.*] == ']') pos.* += 1;
    return arr;
}

fn parseJsonStringLiteral(alloc: Allocator, input: []const u8, pos: *usize) Allocator.Error![]const u8 {
    if (pos.* >= input.len or input[pos.*] != '"') return error.OutOfMemory;
    pos.* += 1; // skip opening "

    var result = std.ArrayListUnmanaged(u8){};

    while (pos.* < input.len and input[pos.*] != '"') {
        if (input[pos.*] == '\\') {
            pos.* += 1;
            if (pos.* >= input.len) break;
            switch (input[pos.*]) {
                '"' => try result.append(alloc, '"'),
                '\\' => try result.append(alloc, '\\'),
                'n' => try result.append(alloc, '\n'),
                'r' => try result.append(alloc, '\r'),
                't' => try result.append(alloc, '\t'),
                '/' => try result.append(alloc, '/'),
                'u' => {
                    // Parse 4 hex digits
                    pos.* += 1;
                    if (pos.* + 4 <= input.len) {
                        const hex = input[pos.* .. pos.* + 4];
                        const codepoint = std.fmt.parseInt(u21, hex, 16) catch 0xFFFD;
                        var buf_arr: [4]u8 = undefined;
                        const len = std.unicode.utf8Encode(codepoint, &buf_arr) catch 0;
                        try result.appendSlice(alloc, buf_arr[0..len]);
                        pos.* += 3; // +1 at end of loop iteration
                    }
                },
                else => {
                    try result.append(alloc, '\\');
                    try result.append(alloc, input[pos.*]);
                },
            }
        } else {
            try result.append(alloc, input[pos.*]);
        }
        pos.* += 1;
    }
    if (pos.* < input.len) pos.* += 1; // skip closing "
    return result.items;
}

fn skipJsonWhitespace(input: []const u8, pos: *usize) void {
    while (pos.* < input.len and (input[pos.*] == ' ' or input[pos.*] == '\t' or input[pos.*] == '\n' or input[pos.*] == '\r')) {
        pos.* += 1;
    }
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

test "escape separator in value" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "equation = x\\=y\n");
    try std.testing.expectEqualStrings("equation", root.entries.items[0].key);
    try std.testing.expectEqualStrings("x=y", root.entries.items[0].value.string);
}

test "escape separator in key" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "key\\=name = value\n");
    try std.testing.expectEqualStrings("key=name", root.entries.items[0].key);
    try std.testing.expectEqualStrings("value", root.entries.items[0].value.string);
}

test "no unescape of non-separator backslash" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "path = C:\\Users\\test\n");
    try std.testing.expectEqualStrings("path", root.entries.items[0].key);
    try std.testing.expectEqualStrings("C:\\Users\\test", root.entries.items[0].value.string);
}

test "header parsing - colon separator" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const config = (try parseHeader(a, "#: kvl 1.0\nhost: localhost\n")).?;
    try std.testing.expectEqualStrings(":", config.separator);
    try std.testing.expect(!config.space_before);
    try std.testing.expect(config.space_after);
}

test "header parsing - arrow separator" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const config = (try parseHeader(a, "#-> kvl 1.0\nname -> test\n")).?;
    try std.testing.expectEqualStrings("->", config.separator);
}

test "header parsing - list markers" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const config = (try parseHeader(a, "#= kvl 1.0 -\ncolors =\n")).?;
    try std.testing.expectEqualStrings("-", config.list_markers);
}

test "colon separator parsing" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "#: kvl 1.0\nhost: localhost\nport: 8080\nurl: https\\://example.com\n");
    try std.testing.expectEqual(@as(usize, 3), root.entries.items.len);
    try std.testing.expectEqualStrings("host", root.entries.items[0].key);
    try std.testing.expectEqualStrings("localhost", root.entries.items[0].value.string);
    try std.testing.expectEqualStrings("port", root.entries.items[1].key);
    try std.testing.expectEqualStrings("8080", root.entries.items[1].value.string);
    try std.testing.expectEqualStrings("url", root.entries.items[2].key);
    try std.testing.expectEqualStrings("https://example.com", root.entries.items[2].value.string);
}

test "compact single value" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "name = Alice\n");
    const compacted = try compact(a, .{ .object = root }, Config{});
    switch (compacted) {
        .object => |obj| {
            try std.testing.expectEqual(@as(usize, 1), obj.entries.items.len);
            try std.testing.expectEqualStrings("name", obj.entries.items[0].key);
            try std.testing.expectEqualStrings("Alice", obj.entries.items[0].value.string);
        },
        else => return error.OutOfMemory,
    }
}

test "compact categorical list" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "tags = web\ntags = api\n");
    const compacted = try compact(a, .{ .object = root }, Config{});
    switch (compacted) {
        .object => |obj| {
            try std.testing.expectEqual(@as(usize, 1), obj.entries.items.len);
            switch (obj.entries.items[0].value) {
                .array => |arr| {
                    try std.testing.expectEqual(@as(usize, 2), arr.items.len);
                    try std.testing.expectEqualStrings("web", arr.items[0].string);
                    try std.testing.expectEqualStrings("api", arr.items[1].string);
                },
                else => return error.OutOfMemory,
            }
        },
        else => return error.OutOfMemory,
    }
}

test "merge two objects" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const obj1 = try parse(a, "name = Alice\n");
    const obj2 = try parse(a, "city = NYC\n");
    const merged = try mergeMaps(a, obj1, obj2);
    try std.testing.expectEqual(@as(usize, 2), merged.entries.items.len);
    try std.testing.expectEqualStrings("name", merged.entries.items[0].key);
    try std.testing.expectEqualStrings("city", merged.entries.items[1].key);
}

test "list marker parsing" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const root = try parse(a, "#= kvl 1.0 -\ncolors =\n    - red\n    - green\n    - blue\n");
    try std.testing.expectEqual(@as(usize, 1), root.entries.items.len);
    try std.testing.expectEqualStrings("colors", root.entries.items[0].key);
    // List items create an array of single-key objects
    const colors = root.entries.items[0].value.array;
    try std.testing.expectEqual(@as(usize, 3), colors.items.len);
    // Each item is {color_name: {}}
    try std.testing.expectEqualStrings("red", colors.items[0].object.entries.items[0].key);
    try std.testing.expectEqualStrings("green", colors.items[1].object.entries.items[0].key);
    try std.testing.expectEqualStrings("blue", colors.items[2].object.entries.items[0].key);
}

test "json roundtrip simple" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const val = try loads(a, "name = Alice\ncity = NYC\n");
    var buf = std.ArrayListUnmanaged(u8){};
    try writeJsonRoot(buf.writer(a), val);

    const expected =
        \\{
        \\  "name": "Alice",
        \\  "city": "NYC"
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, buf.items);
}

test "mixed indentation rejected" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const result = parse(a, "/= Invalid indentation - mixing tabs and spaces\nconfig =\n\thost = localhost\n    port = 8080\n");
    try std.testing.expect(result == ParseError.MixedIndentation);
}
