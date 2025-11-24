const std = @import("std");
const input = @import("../input.zig");

const Day = @import("../day.zig").Day;

pub fn getDay() Day {
    return Day{
        .year = 2022,
        .day = 1,
        .part_one_fn = partOne,
        .part_two_fn = partTwo,
    };
}

fn partOne(allocator: std.mem.Allocator) ![]const u8 {
    var max: i64 = -1;
    var current: i64 = 0;

    var lines = try input.linesIterator(allocator, 2022, 1, "");
    defer lines.deinit();

    while (lines.next()) |line| {
        if (std.mem.eql(u8, line, "")) {
            if (current > max) {
                max = current;
            }

            current = 0;
        } else {
            const calories = try std.fmt.parseInt(i64, line, 10);
            current += calories;
        }
    }

    return try std.fmt.allocPrint(allocator, "{d}", .{max});
}

fn partTwo(allocator: std.mem.Allocator) ![]const u8 {
    var list: std.ArrayList(i64) = .empty;
    defer list.deinit(allocator);

    var current: i64 = 0;
    var lines = try input.linesIterator(allocator, 2022, 1, "");
    defer lines.deinit();

    while (lines.next()) |line| {
        if (std.mem.eql(u8, line, "")) {
            try list.append(allocator, current);
            current = 0;
        } else {
            const calories = try std.fmt.parseInt(i64, line, 10);
            current += calories;
        }
    }

    var elfs = try list.toOwnedSlice(allocator);
    defer allocator.free(elfs);
    std.mem.sort(i64, elfs, {}, comptime std.sort.desc(i64));

    var total: i64 = 0;
    for (elfs[0..3]) |num| {
        total += num;
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
}
