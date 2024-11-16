const std = @import("std");
const print = std.debug.print;

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const linesIterator = @import("../input.zig").linesIterator;

fn build_map(allocator: std.mem.Allocator, input: *const []u8) ![][]u8 {
    var map = std.ArrayList([]u8).init(allocator);
    var lines_iter = linesIterator(input.*);

    var y: usize = 0;
    while (lines_iter.next()) |line| {
        var row = try std.ArrayList(u8).initCapacity(allocator, line.len);

        for (line) |tree| {
            try row.append(tree - '0');
        }

        try map.append(try row.toOwnedSlice());

        y += 1;
    }

    return map.toOwnedSlice();
}

fn id_from_xy(allocator: std.mem.Allocator, x: usize, y: usize) ![]const u8 {
    return try std.fmt.allocPrint(allocator, "{d},{d}", .{ x, y });
}

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    const map = try build_map(allocator, &input);

    var visible = std.StringHashMap(bool).init(allocator);

    // Cast north -> south checks
    for (0..map[0].len) |x| {
        var highest_in_sight: u8 = 0;
        for (0..map.len) |y| {
            const tree = map[y][x];

            if (y == 0 or tree > highest_in_sight) {
                highest_in_sight = tree;
                try visible.put(try id_from_xy(allocator, x, y), true);
            }
        }
    }

    // Cast east -> west checks
    for (0..map.len) |y| {
        var highest_in_sight: u8 = 0;

        var x = map[0].len;
        while (x > 0) {
            x -= 1;

            const tree = map[y][x];
            if (x == map[0].len - 1 or tree > highest_in_sight) {
                highest_in_sight = tree;
                try visible.put(try id_from_xy(allocator, x, y), true);
            }
        }
    }

    // Cast south -> north checks
    for (0..map[0].len) |x| {
        var highest_in_sight: u8 = 0;

        var y = map.len;
        while (y > 0) {
            y -= 1;

            const tree = map[y][x];
            if (y == map.len - 1 or tree > highest_in_sight) {
                highest_in_sight = tree;
                try visible.put(try id_from_xy(allocator, x, y), true);
            }
        }
    }

    // Cast west -> east checks
    for (0..map.len) |y| {
        var highest_in_sight: u8 = 0;

        for (0..map[0].len) |x| {
            const tree = map[y][x];

            if (x == 0 or tree > highest_in_sight) {
                highest_in_sight = tree;
                try visible.put(try id_from_xy(allocator, x, y), true);
            }
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{visible.count()});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = allocator;
    _ = input;

    return "-";
}

pub const task = Task{
    .day = 8,
    .p1 = partOne,
    .p2 = partTwo,
};
