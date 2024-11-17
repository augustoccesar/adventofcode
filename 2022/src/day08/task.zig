const std = @import("std");
const print = std.debug.print;

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const linesIterator = @import("../input.zig").linesIterator;

const Direction = enum {
    north,
    east,
    south,
    west,

    fn modifier(self: Direction) [2]i8 {
        return switch (self) {
            .north => .{ 0, -1 },
            .east => .{ 1, 0 },
            .south => .{ 0, 1 },
            .west => .{ -1, 0 },
        };
    }
};

fn move(map: *const [][]u8, xy: [2]usize, direction: Direction) ?[2]usize {
    const modifier = direction.modifier();
    const x_i8: i8 = @intCast(xy[0]);
    const y_i8: i8 = @intCast(xy[1]);

    const new_x = x_i8 + modifier[0];
    const new_y = y_i8 + modifier[1];

    if (new_x >= map.*[0].len or new_x < 0 or new_y >= map.*.len or new_y < 0) {
        return null;
    }

    return .{
        @intCast(new_x),
        @intCast(new_y),
    };
}

fn build_map(allocator: std.mem.Allocator, input_path: *const []u8) ![][]u8 {
    var map = std.ArrayList([]u8).init(allocator);
    var lines_iter = linesIterator(allocator, input_path.*);

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

fn viewing_distance(map: *const [][]u8, x: usize, y: usize, direction: Direction) u64 {
    const base_height = map.*[y][x];
    var visibles: u64 = 0;

    var current_point = [2]usize{ x, y };
    while (true) {
        if (move(map, current_point, direction)) |new_point| {
            current_point = new_point;

            const new_point_height = map.*[new_point[1]][new_point[0]];

            visibles += 1;

            if (new_point_height >= base_height) {
                break;
            }
        } else {
            break;
        }
    }

    return visibles;
}

fn scenic_score(map: *const [][]u8, x: usize, y: usize) u64 {
    const north = viewing_distance(map, x, y, .north);
    const east = viewing_distance(map, x, y, .east);
    const south = viewing_distance(map, x, y, .south);
    const west = viewing_distance(map, x, y, .west);

    return north * east * south * west;
}

fn partOne(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    const map = try build_map(allocator, &input_path);

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

fn partTwo(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    const map = try build_map(allocator, &input_path);

    var max_scenic: u64 = 0;
    for (0..map.len) |y| {
        for (0..map[0].len) |x| {
            const score = scenic_score(&map, x, y);

            if (score > max_scenic) {
                max_scenic = score;
            }
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{max_scenic});
}

pub const task = Task{
    .day = 8,
    .p1 = partOne,
    .p2 = partTwo,
};
