const std = @import("std");
const kvl = @import("kvl");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    const cmd = args[1];

    if (std.mem.eql(u8, cmd, "parse") or std.mem.eql(u8, cmd, "parse-json")) {
        if (args.len < 3) {
            std.debug.print("Error: parse requires a file path\n", .{});
            std.process.exit(1);
        }
        try parseCmd(alloc, args[2]);
    } else if (std.mem.eql(u8, cmd, "parse-raw")) {
        if (args.len < 3) {
            std.debug.print("Error: parse-raw requires a file path\n", .{});
            std.process.exit(1);
        }
        try parseRawCmd(alloc, args[2]);
    } else if (std.mem.eql(u8, cmd, "serialize")) {
        try serializeCmd(alloc);
    } else if (std.mem.eql(u8, cmd, "merge")) {
        if (args.len < 4) {
            std.debug.print("Error: merge requires two file paths\n", .{});
            std.process.exit(1);
        }
        try mergeCmd(alloc, args[2], args[3]);
    } else {
        std.debug.print("Unknown command: {s}\n", .{cmd});
        printUsage();
        std.process.exit(1);
    }
}

fn parseCmd(alloc: std.mem.Allocator, path: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const content = readFileOrDie(a, path);
    const result = kvl.loads(a, content) catch |err| {
        printParseError(err);
        std.process.exit(1);
    };

    var buf = std.ArrayListUnmanaged(u8){};
    try kvl.writeJsonRoot(buf.writer(a), result);
    writeStdout(buf.items);
}

fn parseRawCmd(alloc: std.mem.Allocator, path: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const content = readFileOrDie(a, path);
    const root = kvl.parse(a, content) catch |err| {
        printParseError(err);
        std.process.exit(1);
    };

    var buf = std.ArrayListUnmanaged(u8){};
    try kvl.writeCategoricalJson(buf.writer(a), root);
    writeStdout(buf.items);
}

fn serializeCmd(alloc: std.mem.Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    // Read JSON from stdin
    const stdin_content = std.fs.File.stdin().readToEndAlloc(a, kvl.MAX_INPUT_SIZE) catch |err| {
        std.debug.print("Error reading stdin: {}\n", .{err});
        std.process.exit(1);
    };

    const val = kvl.parseJson(a, stdin_content) catch |err| {
        std.debug.print("Error parsing JSON: {}\n", .{err});
        std.process.exit(1);
    };

    const output = kvl.serialize(a, val, null) catch |err| {
        std.debug.print("Serialize error: {}\n", .{err});
        std.process.exit(1);
    };

    writeStdout(output);
}

fn mergeCmd(alloc: std.mem.Allocator, path1: []const u8, path2: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const content1 = readFileOrDie(a, path1);
    const content2 = readFileOrDie(a, path2);

    const obj1 = kvl.parse(a, content1) catch |err| {
        printParseError(err);
        std.process.exit(1);
    };
    const obj2 = kvl.parse(a, content2) catch |err| {
        printParseError(err);
        std.process.exit(1);
    };

    const merged = kvl.mergeMaps(a, obj1, obj2) catch |err| {
        std.debug.print("Merge error: {}\n", .{err});
        std.process.exit(1);
    };

    const compacted = kvl.compact(a, .{ .object = merged }, kvl.Config{}) catch |err| {
        std.debug.print("Compact error: {}\n", .{err});
        std.process.exit(1);
    };

    var buf = std.ArrayListUnmanaged(u8){};
    try kvl.writeJsonRoot(buf.writer(a), compacted);
    writeStdout(buf.items);
}

fn readFileOrDie(alloc: std.mem.Allocator, path: []const u8) []const u8 {
    return std.fs.cwd().readFileAlloc(alloc, path, kvl.MAX_INPUT_SIZE) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ path, err });
        std.process.exit(1);
    };
}

fn printParseError(err: anytype) void {
    switch (err) {
        error.InputTooLarge => std.debug.print("Error: Input exceeds maximum size\n", .{}),
        error.MaxDepthExceeded => std.debug.print("Error: Maximum nesting depth exceeded\n", .{}),
        error.MixedIndentation => std.debug.print("Error: Mixed tabs and spaces in indentation\n", .{}),
        error.OutOfMemory => std.debug.print("Error: Out of memory\n", .{}),
    }
}

fn writeStdout(data: []const u8) void {
    const stdout = std.fs.File.stdout();
    stdout.writeAll(data) catch |err| {
        std.debug.print("Write error: {}\n", .{err});
        std.process.exit(1);
    };
}

fn printUsage() void {
    std.debug.print(
        \\Usage: kvl <command> [args...]
        \\
        \\Commands:
        \\  parse <file>           Parse KVL file to compacted JSON
        \\  parse-raw <file>       Parse KVL file to categorical JSON
        \\  serialize              Read JSON from stdin, write KVL to stdout
        \\  merge <file1> <file2>  Merge two KVL files to compacted JSON
        \\
    , .{});
}
