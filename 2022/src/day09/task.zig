const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = allocator;
    _ = input;

    return "-";
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = allocator;
    _ = input;

    return "-";
}

pub const task = Task{
    .day = 9,
    .p1 = partOne,
    .p2 = partTwo,
};
