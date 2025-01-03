const std = @import("std");
const print = std.debug.print;

const helpers = @import("../helpers.zig");

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const Direction = enum {
    const Self = @This();

    up,
    down,
    left,
    right,

    fn modifier(self: Self) [2]i8 {
        return switch (self) {
            Self.up => .{ 0, -1 },
            Self.down => .{ 0, 1 },
            Self.left => .{ -1, 0 },
            Self.right => .{ 1, 0 },
        };
    }

    fn all() [4]Direction {
        return [4]Direction{ .up, .down, .left, .right };
    }
};

const Map = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    inner: [][]u8,
    start: Coord,
    target: Coord,

    fn init(allocator: std.mem.Allocator, data: []const u8) !Self {
        var map = std.ArrayList([]u8).init(allocator);
        var lines_iter = std.mem.splitScalar(u8, data, '\n');

        var start = Coord{ .x = 0, .y = 0 };
        var target = Coord{ .x = 0, .y = 0 };

        var y: usize = 0;
        while (lines_iter.next()) |line| {
            var row = try std.ArrayList(u8).initCapacity(allocator, line.len);

            for (line, 0..) |height, x| {
                switch (height) {
                    'S' => {
                        start = .{ .x = @intCast(x), .y = @intCast(y) };
                        try row.append(0);
                    },
                    'E' => {
                        target = .{ .x = @intCast(x), .y = @intCast(y) };
                        try row.append('z' - 'a');
                    },
                    else => try row.append(height - 'a'),
                }
            }

            try map.append(try row.toOwnedSlice());

            y += 1;
        }

        return Self{
            .allocator = allocator,
            .inner = try map.toOwnedSlice(),
            .start = start,
            .target = target,
        };
    }

    fn at(self: *const Self, coord: *const Coord) ?u8 {
        if (coord.x < 0 or coord.y < 0 or coord.x >= self.inner[0].len or coord.y >= self.inner.len) {
            return null;
        }

        const x_usize = @as(usize, @intCast(coord.x));
        const y_usize = @as(usize, @intCast(coord.y));

        return self.inner[y_usize][x_usize];
    }

    fn neighbors(self: *const Self, pos: Coord) ![]Coord {
        var possible_neighbors = try std.ArrayList(Coord).initCapacity(self.allocator, 4);

        for (Direction.all()) |direction| {
            const new_coord = pos.move(direction);

            if (self.at(&new_coord)) |new_coord_height| {
                const current_pos_height = self.at(&pos).?;

                if (new_coord_height <= current_pos_height + 1) {
                    try possible_neighbors.append(new_coord);
                }
            }
        }

        return possible_neighbors.toOwnedSlice();
    }

    fn possible_starts(self: *const Self) ![]Coord {
        var starts = std.ArrayList(Coord).init(self.allocator);

        for (0..self.inner.len) |y| {
            for (0..self.inner[0].len) |x| {
                if (self.inner[y][x] == 0) {
                    try starts.append(Coord{
                        .x = @intCast(x),
                        .y = @intCast(y),
                    });
                }
            }
        }

        return starts.toOwnedSlice();
    }
};

const Coord = struct {
    const Self = @This();

    x: i8,
    y: i8,

    fn move(self: Self, direction: Direction) Self {
        const modifier = direction.modifier();

        return .{
            .x = self.x + modifier[0],
            .y = self.y + modifier[1],
        };
    }

    fn key(self: Self, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "{d},{d}", .{ self.x, self.y });
    }

    fn distance(self: *const Self, other: *const Self) usize {
        const res = @abs(self.x - other.x) + @abs(self.y - other.y);

        return res;
    }
};

const FrontierCoord = struct {
    coord: Coord,
    cost: usize,
};

fn frontier_fn(_: void, coord_a: FrontierCoord, coord_b: FrontierCoord) std.math.Order {
    return std.math.order(coord_a.cost, coord_b.cost);
}

fn walk_to_target(allocator: std.mem.Allocator, map: *const Map, from: Coord) !usize {
    var frontier = std.PriorityQueue(FrontierCoord, void, frontier_fn).init(allocator, {});
    try frontier.add(FrontierCoord{ .coord = from, .cost = from.distance(&map.target) });

    var costs = std.StringHashMap(usize).init(allocator);
    try costs.put(try from.key(allocator), 0);

    var came_from = std.StringHashMap(?Coord).init(allocator);
    try came_from.put(try from.key(allocator), null);

    while (frontier.count() > 0) {
        const current = frontier.remove().coord;

        if (current.x == map.target.x and current.y == map.target.y) {
            break;
        }

        const current_key = try current.key(allocator);
        const current_cost = costs.get(current_key).?;

        for (try map.neighbors(current)) |neighbor| {
            const cost_to_neighbor = current_cost + 1;
            const neighbor_key = try neighbor.key(allocator);

            if ((costs.get(neighbor_key) orelse std.math.maxInt(usize)) > cost_to_neighbor) {
                try costs.put(neighbor_key, cost_to_neighbor);
                try came_from.put(neighbor_key, current);

                const neighbor_total_cost = cost_to_neighbor + neighbor.distance(&map.target);
                try frontier.add(FrontierCoord{ .coord = neighbor, .cost = neighbor_total_cost });
            }
        }
    }

    var current: Coord = map.target;
    var path = std.ArrayList(Coord).init(allocator);

    while (current.x != from.x or current.y != from.y) {
        try path.append(current);
        const came_from_pos = came_from.get(try current.key(allocator)) orelse return std.math.maxInt(usize);

        current = came_from_pos.?;
    }

    return path.items.len;
}

fn partOne(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    const map_data = helpers.input.readString(allocator, input_path);
    const map = try Map.init(allocator, map_data);

    const steps = try walk_to_target(allocator, &map, map.start);

    return std.fmt.allocPrint(allocator, "{d}", .{steps});
}

// TODO: Improve this. It is very slow...
fn partTwo(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    const map_data = helpers.input.readString(allocator, input_path);
    const map = try Map.init(allocator, map_data);

    var shortest_path: usize = std.math.maxInt(usize);
    for (try map.possible_starts()) |start| {
        const steps = try walk_to_target(allocator, &map, start);
        if (steps < shortest_path) {
            shortest_path = steps;
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{shortest_path});
}

pub const task = Task{
    .day = 12,
    .p1 = partOne,
    .p2 = partTwo,
};
