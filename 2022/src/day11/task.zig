const std = @import("std");
const print = std.debug.print;

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const linesIterator = @import("../input.zig").linesIterator;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = input;

    return std.fmt.allocPrint(allocator, "-", .{});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = input;

    return std.fmt.allocPrint(allocator, "-", .{});
}

pub const task = Task{
    .day = 11,
    .p1 = partOne,
    .p2 = partTwo,
};
