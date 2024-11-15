const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const linesIterator = @import("../input.zig").linesIterator;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var total: u64 = 0;
    var rucksacks = linesIterator(input);
    while (rucksacks.next()) |rucksack| {
        const mid = rucksack.len / 2;
        const left_compartment = rucksack[0..mid];
        const right_compartment = rucksack[mid..];

        var match: u8 = 0;
        for (left_compartment) |item_left| {
            for (right_compartment) |item_right| {
                if (item_left == item_right) {
                    match = item_left;
                    break;
                }
            }

            if (match > 0) break;
        }

        const value = item_value(match);

        total += value;
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = allocator;
    _ = input;

    return "-";
}

pub const task = Task{
    .day = 3,
    .p1 = partOne,
    .p2 = partTwo,
};

fn item_value(item: u8) u8 {
    return switch (item) {
        'a'...'z' => item - ('a' - 1),
        'A'...'Z' => 26 + item - ('A' - 1),
        else => @panic("invalid value for item"),
    };
}
