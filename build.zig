const std = @import("std");

/// Repo-root re-export package.  The real Zig implementation lives in `zig/`
/// (its own package, module `kvl`).  This root package makes the whole repo a
/// valid Zig package too, so a consumer can `zig fetch git+.../kvl` and import
/// `kvl` without reaching into the `zig/` subdir.  It re-exposes the inner
/// module rather than redefining it, so there is one source of truth.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const inner = b.dependency("kvl_inner", .{ .target = target });
    b.modules.put(b.dupe("kvl"), inner.module("kvl")) catch @panic("OOM");
}
