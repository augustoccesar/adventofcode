const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const linesIterator = @import("../input.zig").linesIterator;

fn partOne(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    var total: u64 = 0;
    var rucksacks = linesIterator(allocator, input_path);
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

fn partTwo(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    var total: u64 = 0;
    var rucksacks = linesIterator(allocator, input_path);
    while (rucksacks.next()) |rucksack| {
        const elf_1 = rucksack;
        const elf_2 = rucksacks.next() orelse @panic("failed to get rucksack for elf 2");
        const elf_3 = rucksacks.next() orelse @panic("failed to get rucksack for elf 3");

        const elfs = [3][]const u8{ elf_1, elf_2, elf_3 };

        var items_tracker = std.AutoHashMap(u8, [3]bool).init(allocator);
        defer items_tracker.deinit();

        var matching_item: u8 = 0;
        for (0..elfs.len) |i| {
            for (elfs[i]) |item| {
                var count = items_tracker.get(item) orelse [3]bool{ false, false, false };
                count[i] = true;

                if (count[0] and count[1] and count[2]) {
                    matching_item = item;
                    break;
                }

                try items_tracker.put(item, count);
            }

            if (matching_item > 0) break;
        }

        total += item_value(matching_item);
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
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
