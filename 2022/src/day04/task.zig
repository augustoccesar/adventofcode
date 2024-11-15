const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const linesIterator = @import("../input.zig").linesIterator;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var total: u64 = 0;

    var lines = linesIterator(input);
    while (lines.next()) |line| {
        const assignments = try parse_line(line);

        if (assignments[0].full_overlap(assignments[1])) {
            total += 1;
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var total: u64 = 0;

    var lines = linesIterator(input);
    while (lines.next()) |line| {
        const assignments = try parse_line(line);

        if (assignments[0].any_overlap(assignments[1])) {
            total += 1;
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
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

    fn full_overlap(self: Assignment, other: Assignment) bool {
        const sorted = Assignment.sort(self, other);

        return sorted[0].from <= sorted[1].from and sorted[0].to >= sorted[1].to;
    }

    fn any_overlap(self: Assignment, other: Assignment) bool {
        const sorted = Assignment.sort(self, other);

        return sorted[1].from >= sorted[0].from and sorted[1].from <= sorted[0].to;
    }

    fn sort(self: Assignment, other: Assignment) [2]Assignment {
        var left: Assignment = self;
        var right: Assignment = other;

        if ((self.from == other.from and self.to > other.to) or (self.from < other.from)) {
            left = self;
            right = other;
        } else {
            left = other;
            right = self;
        }

        return .{ left, right };
    }
};

fn parse_line(line: []const u8) ![2]Assignment {
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

    return .{
        .{ .from = assignments_raw[0][0], .to = assignments_raw[0][1] },
        .{ .from = assignments_raw[1][0], .to = assignments_raw[1][1] },
    };
}
