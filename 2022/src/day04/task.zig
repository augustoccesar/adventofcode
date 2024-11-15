const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const linesIterator = @import("../input.zig").linesIterator;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var total: u64 = 0;

    var lines = linesIterator(input);
    while (lines.next()) |line| {
        var assignments_raw: [2][2]u8 = [2][2]u8{
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

                assignments_raw[section_idx][id_idx] = value;
                id_idx += 1;
            }
            section_idx += 1;
        }

        const assignments: [2]Assignment = [2]Assignment{
            Assignment{ .from = assignments_raw[0][0], .to = assignments_raw[0][1] },
            Assignment{ .from = assignments_raw[1][0], .to = assignments_raw[1][1] },
        };

        if (assignments[0].contains(assignments[1])) {
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

const Assignment = struct {
    from: u8,
    to: u8,

    fn size(self: Assignment) u8 {
        return self.to - self.from;
    }

    fn contains(self: Assignment, other: Assignment) bool {
        var left: ?Assignment = null;
        var right: ?Assignment = null;

        if ((self.from == other.from and self.to > other.to) or (self.from < other.from)) {
            left = self;
            right = other;
        } else {
            left = other;
            right = self;
        }

        return left.?.from <= right.?.from and left.?.to >= right.?.to;
    }
};
