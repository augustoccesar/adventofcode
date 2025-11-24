const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "aoc-cpp",
        .root_module = b.addModule("root", .{
            .target = target,
            .optimize = optimize,
        }),
    });

    exe.addCSourceFiles(.{
        .files = &[_][]const u8{
            "src/main.cpp",
            "src/Day.cpp",
            "src/commands/RunCommand.cpp",
            "src/y2017/d01.cpp",
            // CODEGEN:register_day
        },
        .flags = &[_][]const u8{
            "-std=c++20",
            "-Wall",
            "-Wextra",
        },
    });

    exe.linkLibCpp();
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
