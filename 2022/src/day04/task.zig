const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const linesIterator = @import("../input.zig").linesIterator;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var total: u64 = 0;

    var lines = linesIterator(input);
    while (lines.next()) |line| {
        var assignments: [2][2]u8 = [2][2]u8{
            [2]u8{ 0, 0 },
            [2]u8{ 0, 0 },
        };

        var sections = std.mem.splitAny(u8, line, ",");
        var section_idx: usize = 0;
        while (sections.next()) |section| {
            var ids = std.mem.splitAny(u8, section, "-");
            var id_idx: usize = 0;
            while (ids.next()) |id| {
                const value = try std.fmt.parseInt(u8, id, 10);

                assignments[section_idx][id_idx] = value;
                id_idx += 1;
            }
            section_idx += 1;
        }

        var left_assignment: ?[2]u8 = null;
        var right_assignment: ?[2]u8 = null;

        if (assignments[0][0] > assignments[1][0]) {
            left_assignment = assignments[1];
            right_assignment = assignments[0];
        } else if (assignments[0][0] == assignments[1][0]) {
            if (assignments[0][1] > assignments[1][1]) {
                left_assignment = assignments[0];
                right_assignment = assignments[1];
            } else {
                left_assignment = assignments[1];
                right_assignment = assignments[0];
            }
        } else {
            left_assignment = assignments[0];
            right_assignment = assignments[1];
        }

        const overlap = left_assignment.?[0] <= right_assignment.?[0] and left_assignment.?[1] >= right_assignment.?[1];

        if (overlap) {
            total += 1;
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = allocator;
    _ = input;

    return "-";
}

pub const task = Task{
    .day = 4,
    .p1 = partOne,
    .p2 = partTwo,
};
