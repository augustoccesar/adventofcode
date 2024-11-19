const std = @import("std");
const print = std.debug.print;

const helpers = @import("../helpers.zig");

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

fn partOne(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    _ = input_path;

    return std.fmt.allocPrint(allocator, "-", .{});
}

fn partTwo(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    _ = input_path;

    return std.fmt.allocPrint(allocator, "-", .{});
}

pub const task = Task{
    .day = 12,
    .p1 = partOne,
    .p2 = partTwo,
};
