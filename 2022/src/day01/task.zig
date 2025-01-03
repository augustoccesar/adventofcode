const std = @import("std");

const helpers = @import("../helpers.zig");

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

fn partOne(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    var max: i64 = -1;
    var current: i64 = 0;

    var lines = helpers.input.linesIterator(allocator, input_path);
    while (lines.next()) |line| {
        if (std.mem.eql(u8, line, "")) {
            if (current > max) {
                max = current;
            }

            current = 0;
        } else {
            const calories = try std.fmt.parseInt(i64, line, 10);
            current += calories;
        }
    }

    return try std.fmt.allocPrint(allocator, "{d}", .{max});
}

fn partTwo(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    var list = std.ArrayList(i64).init(allocator);
    defer list.deinit();

    var current: i64 = 0;
    var lines = helpers.input.linesIterator(allocator, input_path);
    while (lines.next()) |line| {
        if (std.mem.eql(u8, line, "")) {
            try list.append(current);
            current = 0;
        } else {
            const calories = try std.fmt.parseInt(i64, line, 10);
            current += calories;
        }
    }

    var elfs = try list.toOwnedSlice();
    std.mem.sort(i64, elfs, {}, comptime std.sort.desc(i64));

    var total: i64 = 0;
    for (elfs[0..3]) |num| {
        total += num;
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
}

pub const task = Task{
    .day = 1,
    .p1 = partOne,
    .p2 = partTwo,
};
