const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const linesIterator = @import("../input.zig").linesIterator;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var result: usize = 0;
    for (3..input.len) |i| {
        const buffer = input[(i - 3)..(i + 1)];

        if (all_different(buffer)) {
            result = i + 1;
            break;
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{result});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = allocator;
    _ = input;

    return "-";
}

pub const task = Task{
    .day = 6,
    .p1 = partOne,
    .p2 = partTwo,
};

fn all_different(slice: []const u8) bool {
    for (0..slice.len) |i| {
        for ((i + 1)..slice.len) |j| {
            if (slice[i] == slice[j]) {
                return false;
            }
        }
    }

    return true;
}
