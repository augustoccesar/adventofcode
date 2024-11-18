const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const gen_exe = b.addExecutable(.{
        .name = "gen_exe",
        .root_source_file = b.path("src/gen.zig"),
        .target = target,
        .optimize = optimize,
    });

    const days_gen_exe = b.addRunArtifact(gen_exe);
    days_gen_exe.step.dependOn(&gen_exe.step);

    const output_file = days_gen_exe.addOutputFileArg("days.zig");
    const gen_write_files = b.addUpdateSourceFiles();
    _ = gen_write_files.addCopyFileToSource(output_file, "src/days.zig");

    const exe = b.addExecutable(.{
        .name = "2022",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.step.dependOn(&gen_write_files.step);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
