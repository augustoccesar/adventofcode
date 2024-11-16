const std = @import("std");
const print = std.debug.print;

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const linesIterator = @import("../input.zig").linesIterator;

const Direction = enum {
    up,
    down,
    left,
    right,

    fn from_u8(value: u8) Direction {
        return switch (value) {
            'U' => .up,
            'D' => .down,
            'L' => .left,
            'R' => .right,
            else => unreachable,
        };
    }

    fn modifer(self: Direction) [2]i8 {
        return switch (self) {
            .up => [2]i8{ 0, 1 },
            .down => [2]i8{ 0, -1 },
            .left => [2]i8{ -1, 0 },
            .right => [2]i8{ 1, 0 },
        };
    }

    fn apply_to(self: Direction, xy: [2]i16) [2]i16 {
        const modifier = self.modifer();

        return .{
            xy[0] + modifier[0],
            xy[1] + modifier[1],
        };
    }
};

fn is_touching(xy_1: [2]i16, xy_2: [2]i16) bool {
    return @abs(xy_1[0] - xy_2[0]) < 2 and @abs(xy_1[1] - xy_2[1]) < 2;
}

fn xy_id(allocator: std.mem.Allocator, xy: [2]i16) ![]const u8 {
    return try std.fmt.allocPrint(allocator, "{d},{d}", .{ xy[0], xy[1] });
}

fn move_towards(from: [2]i16, to: [2]i16) [2]i16 {
    if (from[0] < to[0] and from[1] == to[1]) {
        // In same y. Need to move +1 on x
        return .{ from[0] + 1, from[1] };
    } else if (from[0] > to[0] and from[1] == to[1]) {
        // In same y. Need to move -1 on x
        return .{ from[0] - 1, from[1] };
    } else if (from[0] == to[0] and from[1] < to[1]) {
        // in same x. Need to move +1 on y
        return .{ from[0], from[1] + 1 };
    } else if (from[0] == to[0] and from[1] > to[1]) {
        // in same x. Need to move -1 on y.
        return .{ from[0], from[1] - 1 };
    } else if (from[0] < to[0] and from[1] < to[1]) {
        // Need to move +1 on x and y
        return .{ from[0] + 1, from[1] + 1 };
    } else if (from[0] < to[0] and from[1] > to[1]) {
        // Need to move +1 on x and -1 on y
        return .{ from[0] + 1, from[1] - 1 };
    } else if (from[0] > to[0] and from[1] > to[1]) {
        // Need to move -1 on x and -1 on y
        return .{ from[0] - 1, from[1] - 1 };
    } else if (from[0] > to[0] and from[1] < to[1]) {
        // Need to move -1 on x and +1 on y
        return .{ from[0] - 1, from[1] + 1 };
    }

    unreachable;
}

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var head = [2]i16{ 0, 0 };
    var tail = [2]i16{ 0, 0 };

    var position_tracker = std.StringHashMap(bool).init(allocator);
    defer position_tracker.deinit();

    try position_tracker.put(try xy_id(allocator, tail), true);

    var lines_iter = linesIterator(input);
    while (lines_iter.next()) |line| {
        var parts = std.mem.splitAny(u8, line, " ");
        const direction = Direction.from_u8(parts.next().?[0]);
        const steps = try std.fmt.parseInt(u8, parts.next().?, 10);

        for (0..steps) |_| {
            const prev_head_pos = head;
            head = direction.apply_to(head);

            if (!is_touching(head, tail)) {
                tail = prev_head_pos;
                try position_tracker.put(try xy_id(allocator, tail), true);
            }
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{position_tracker.count()});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var rope = [10][2]i16{
        .{ 0, 0 },
        .{ 0, 0 },
        .{ 0, 0 },
        .{ 0, 0 },
        .{ 0, 0 },
        .{ 0, 0 },
        .{ 0, 0 },
        .{ 0, 0 },
        .{ 0, 0 },
        .{ 0, 0 },
    };
    const head = &rope[0];
    const tail = &rope[9];

    var position_tracker = std.StringHashMap(bool).init(allocator);
    defer position_tracker.deinit();

    try position_tracker.put(try xy_id(allocator, tail.*), true);

    var lines_iter = linesIterator(input);
    while (lines_iter.next()) |line| {
        var parts = std.mem.splitAny(u8, line, " ");
        const direction = Direction.from_u8(parts.next().?[0]);
        const steps = try std.fmt.parseInt(u8, parts.next().?, 10);

        for (0..steps) |_| {
            head.* = direction.apply_to(head.*);

            var previous_node = head.*;
            for (1..10) |tail_idx| {
                const current_node = &rope[tail_idx];
                if (!is_touching(previous_node, current_node.*)) {
                    current_node.* = move_towards(current_node.*, previous_node);
                    if (tail_idx == 9) {
                        try position_tracker.put(try xy_id(allocator, current_node.*), true);
                    }
                }

                previous_node = current_node.*;
            }
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{position_tracker.count()});
}

pub const task = Task{
    .day = 9,
    .p1 = partOne,
    .p2 = partTwo,
};
