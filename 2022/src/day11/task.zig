const std = @import("std");
const print = std.debug.print;

const helpers = @import("../helpers.zig");

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const OperationType = enum {
    const Self = @This();

    add,
    mult,

    fn from_u8(value: u8) Self {
        return switch (value) {
            '+' => Self.add,
            '*' => Self.mult,
            else => @panic("received unknown operation type"),
        };
    }
};

const Operation = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    ty: OperationType,
    value: ?u64, // If null, is 'old'

    fn parse(allocator: std.mem.Allocator, data: []const u8) !*Self {
        var operation_parts = std.mem.splitScalar(u8, data, ' ');
        const operation_type = OperationType.from_u8(operation_parts.next().?[0]);
        const value_raw = operation_parts.next().?;

        const value: ?u64 = if (std.mem.eql(u8, value_raw, "old")) null else try std.fmt.parseInt(u64, value_raw, 10);

        const operation = try allocator.create(Self);
        operation.* = .{
            .allocator = allocator,
            .ty = operation_type,
            .value = value,
        };

        return operation;
    }

    fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }
};

const Monkey = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    idx: usize,
    holding_items: std.ArrayList(u64),
    operation: *Operation,
    test_value: u64,
    true_idx: usize,
    false_idx: usize,

    fn parse(allocator: std.mem.Allocator, data: []const u8) !*Self {
        var parts = std.mem.splitAny(u8, data, "\n");
        const monkey_idx = parts.next().?[7] - '0';

        var items_parts = std.mem.splitBackwardsScalar(u8, parts.next().?, ':');
        var items_raw_parts = std.mem.splitScalar(u8, items_parts.next().?[1..], ',');
        var items = std.ArrayList(u64).init(allocator);
        while (items_raw_parts.next()) |item_raw| {
            try items.append(try std.fmt.parseInt(u64, std.mem.trim(u8, item_raw, " "), 10));
        }

        var operation_parts = std.mem.splitBackwardsScalar(u8, parts.next().?, '=');
        const operation_raw = operation_parts.next().?[5..];

        var test_parts = std.mem.splitBackwardsScalar(u8, parts.next().?, ' ');
        const test_value = try std.fmt.parseInt(u64, test_parts.next().?, 10);

        var true_target_parts = std.mem.splitBackwardsScalar(u8, parts.next().?, ' ');
        const true_target = true_target_parts.next().?[0] - '0';

        var false_target_parts = std.mem.splitBackwardsScalar(u8, parts.next().?, ' ');
        const false_target = false_target_parts.next().?[0] - '0';

        const monkey = try allocator.create(Self);
        monkey.* = .{
            .allocator = allocator,
            .idx = monkey_idx,
            .holding_items = items,
            .operation = try Operation.parse(allocator, operation_raw),
            .test_value = test_value,
            .true_idx = true_target,
            .false_idx = false_target,
        };

        return monkey;
    }

    fn deinit(self: *Self) void {
        self.holding_items.deinit();
        self.operation.deinit();

        self.allocator.destroy(self);
    }
};

fn run_simulation(allocator: std.mem.Allocator, input_path: []u8, target_rounds: u64) TaskError![]const u8 {
    const input = helpers.input.readString(allocator, input_path);
    var monkey_data = std.mem.splitSequence(u8, input, "\n\n");

    var monkeys = std.ArrayList(*Monkey).init(allocator);
    defer {
        for (monkeys.items) |monkey| monkey.deinit();
        monkeys.deinit();
    }

    var control_mod: u64 = 1;

    while (monkey_data.next()) |data| {
        const monkey = try Monkey.parse(allocator, data);
        try monkeys.append(monkey);

        control_mod *= monkey.test_value;
    }

    var inspections = try std.ArrayList(u64).initCapacity(allocator, monkeys.items.len);
    defer inspections.deinit();
    for (0..monkeys.items.len) |_| {
        try inspections.append(0);
    }

    var round: u64 = 0;
    while (round < target_rounds) : (round += 1) {
        for (monkeys.items) |monkey| {
            while (monkey.holding_items.items.len > 0) {
                var item = monkey.holding_items.orderedRemove(0);

                inspections.items[monkey.idx] += 1;

                switch (monkey.operation.ty) {
                    .add => item += monkey.operation.value orelse item,
                    .mult => item *= monkey.operation.value orelse item,
                }

                if (target_rounds > 20) {
                    item %= control_mod;
                } else {
                    item /= 3;
                }

                if (item % monkey.test_value == 0) {
                    try monkeys.items[monkey.true_idx].holding_items.append(item);
                } else {
                    try monkeys.items[monkey.false_idx].holding_items.append(item);
                }
            }
        }
    }

    const inspections_slice = inspections.items;
    std.mem.sort(u64, inspections_slice, {}, comptime std.sort.desc(u64));
    const result = inspections_slice[0] * inspections_slice[1];

    return std.fmt.allocPrint(allocator, "{d}", .{result});
}

fn partOne(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    return try run_simulation(allocator, input_path, 20);
}

fn partTwo(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    return try run_simulation(allocator, input_path, 10_000);
}

pub const task = Task{
    .day = 11,
    .p1 = partOne,
    .p2 = partTwo,
};
