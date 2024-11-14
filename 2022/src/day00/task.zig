const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const linesIterator = @import("../input.zig").linesIterator;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var dynamic_string = std.ArrayList(u8).init(allocator);
    defer dynamic_string.deinit();

    var string_writer = dynamic_string.writer();

    var lines = linesIterator(input);
    while (lines.next()) |line| {
        try string_writer.writeAll(line);
    }

    const res = try dynamic_string.toOwnedSlice();
    return res;
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var res: u64 = 0;

    var lines = linesIterator(input);
    while (lines.next()) |line| {
        const value = try std.fmt.parseInt(u8, line, 10);
        res += value;
    }

    const resStr = try std.fmt.allocPrint(allocator, "{d}", .{res});

    return resStr;
}

pub const task = Task{
    .day = 0,
    .p1 = partOne,
    .p2 = partTwo,
};
