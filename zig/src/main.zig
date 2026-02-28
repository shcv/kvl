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

    if (std.mem.eql(u8, cmd, "parse-json")) {
        if (args.len < 3) {
            std.debug.print("Error: parse-json requires a file path\n", .{});
            std.process.exit(1);
        }
        try parseJsonCmd(alloc, args[2]);
    } else {
        std.debug.print("Unknown command: {s}\n", .{cmd});
        printUsage();
        std.process.exit(1);
    }
}

fn parseJsonCmd(alloc: std.mem.Allocator, path: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const content = std.fs.cwd().readFileAlloc(a, path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ path, err });
        std.process.exit(1);
    };

    const root = kvl.parse(a, content) catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        std.process.exit(1);
    };

    // Accumulate JSON output in a buffer, then write to stdout
    var buf: std.ArrayList(u8) = .{};
    defer buf.deinit(alloc);
    try kvl.writeCompactedJson(buf.writer(alloc), root);

    // Write to stdout
    const out = buf.items;
    _ = std.posix.write(std.posix.STDOUT_FILENO, out) catch |err| {
        std.debug.print("Write error: {}\n", .{err});
        std.process.exit(1);
    };
}

fn printUsage() void {
    std.debug.print(
        \\Usage: kvl-demo <command> [args...]
        \\
        \\Commands:
        \\  parse-json <file>    Parse a KVL file and output compacted JSON
        \\
    , .{});
}
