const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addSharedLibrary(.{
        .name = "metalwrapper",
        .target = target,
        .optimize = optimize,
    });
    lib.addCSourceFile("src/metal_wrapper.m", &[_][]const u8{});
    lib.linkFramework("QuartzCore");
    b.installArtifact(lib);
}
