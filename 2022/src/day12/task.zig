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

const Frontier = struct {
    const Self = @This();
    const Item = struct { coord: Coord, cost: usize };

    allocator: std.mem.Allocator,
    arr: std.ArrayList(Item),

    fn init(allocator: std.mem.Allocator) Self {
        const arr = std.ArrayList(Item).init(allocator);

        return .{
            .allocator = allocator,
            .arr = arr,
        };
    }

    fn add(self: *Frontier, coord: Coord, cost: usize) !void {
        const new_item = Item{
            .coord = coord,
            .cost = cost,
        };

        try self.*.arr.append(new_item);
    }

    // TODO: Improve this one. Probably can have frontier be a linked list.
    fn pop(self: *Frontier) ?Coord {
        if (self.arr.items.len == 0) {
            return null;
        }

        var lowest_cost_idx: usize = 0;
        for (self.arr.items, 0..) |item, i| {
            if (item.cost < self.arr.items[lowest_cost_idx].cost) {
                lowest_cost_idx = i;
            }
        }

        return self.arr.orderedRemove(lowest_cost_idx).coord;
    }
};

fn partOne(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    const map_data = helpers.input.readString(allocator, input_path);
    const map = try Map.init(allocator, map_data);

    var frontier = Frontier.init(allocator);
    try frontier.add(map.start, map.start.distance(&map.target));

    var costs = std.StringHashMap(usize).init(allocator);
    try costs.put(try map.start.key(allocator), 0);

    var came_from = std.StringHashMap(?Coord).init(allocator);
    try came_from.put(try map.start.key(allocator), null);

    while (frontier.pop()) |current| {
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
                try frontier.add(neighbor, neighbor_total_cost);
            }
        }
    }

    const steps = track_back: {
        var current: Coord = map.target;
        var path = std.ArrayList(Coord).init(allocator);

        while (current.x != map.start.x or current.y != map.start.y) {
            try path.append(current);
            const came_from_pos = came_from.get(try current.key(allocator)).?.?;

            current = came_from_pos;
        }

        break :track_back path.items.len;
    };

    return std.fmt.allocPrint(allocator, "{d}", .{steps});
}

fn partTwo(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    _ = input_path;

    return std.fmt.allocPrint(allocator, "-", .{});
}

pub const task = Task{
    .day = 12,
    .p1 = partOne,
    .p2 = partTwo,
};
