const std = @import("std");
const print = std.debug.print;

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const linesIterator = @import("../input.zig").linesIterator;

const Point2D = struct {
    x: i8,
    y: i8,

    fn from_xy(x: usize, y: usize) Point2D {
        return .{
            .x = @intCast(x),
            .y = @intCast(y),
        };
    }

    fn to_xy(self: Point2D) std.meta.Tuple(&.{ usize, usize }) {
        if (self.x < 0 or self.y < 0) {
            @panic("oops");
        }

        return .{ @intCast(self.x), @intCast(self.y) };
    }

    fn move(self: Point2D, direction: Direction) Point2D {
        const modifier = direction.modifier();

        return .{
            .x = self.x + modifier.x,
            .y = self.y + modifier.y,
        };
    }
};

const Direction = enum {
    north,
    east,
    south,
    west,

    fn modifier(self: Direction) Point2D {
        return switch (self) {
            .north => Point2D{ .x = 0, .y = -1 },
            .east => Point2D{ .x = 1, .y = 0 },
            .south => Point2D{ .x = 0, .y = 1 },
            .west => Point2D{ .x = -1, .y = 0 },
        };
    }
};

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

fn walk(map: *const [][]u8, x: usize, y: usize, direction: Direction) u64 {
    const base_height = map.*[y][x];
    var visibles: u64 = 0;

    var current_point = Point2D.from_xy(x, y);
    while (true) {
        const new_point = current_point.move(direction);
        if (new_point.x >= map.*[0].len or new_point.x < 0 or new_point.y >= map.*.len or new_point.y < 0) {
            break;
        }

        current_point = new_point;

        const xy = new_point.to_xy();
        const new_point_height = map.*[xy[1]][xy[0]];

        visibles += 1;

        if (new_point_height >= base_height) {
            break;
        }
    }

    return visibles;
}

fn scenic_score(map: *const [][]u8, x: usize, y: usize) u64 {
    const north = walk(map, x, y, .north);
    const east = walk(map, x, y, .east);
    const south = walk(map, x, y, .south);
    const west = walk(map, x, y, .west);

    return north * east * south * west;
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
    const map = try build_map(allocator, &input);

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
