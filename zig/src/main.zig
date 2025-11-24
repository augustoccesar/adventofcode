const std = @import("std");

const day = @import("day.zig");
const y2022d01 = @import("y2022/d01.zig");
// CODEGEN:import_day

const DayKey = struct {
    year: u16,
    day: u8,

    pub fn eql(self: DayKey, other: DayKey) bool {
        return self.year == other.year and self.day == other.day;
    }

    pub fn hash(self: DayKey) u64 {
        return (@as(u64, self.year) << 32) | @as(u64, self.day);
    }
};

const DayMap = std.HashMap(DayKey, day.Day, std.hash_map.AutoContext(DayKey), std.hash_map.default_max_load_percentage);

const registered_days = [_]day.Day{
    y2022d01.getDay(),
    // CODEGEN:register_day
};

fn createDaysMap(allocator: std.mem.Allocator) !DayMap {
    var map = DayMap.init(allocator);

    for (registered_days) |dayInstance| {
        try map.put(DayKey{ .year = dayInstance.year, .day = dayInstance.day }, dayInstance);
    }

    return map;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Invalid arguments", .{});
        return;
    }

    const command = args[1];
    if (std.mem.eql(u8, command, "run")) {
        try run_command(allocator, args[2..]);
    }
}

fn run_command(allocator: std.mem.Allocator, args: [][:0]u8) !void {
    if (args.len < 2) {
        std.debug.print("Invalid number of arguments", .{});
        std.process.exit(1);
    }

    const yearInput = std.fmt.parseInt(u16, args[0], 10) catch |err| {
        std.debug.print("Invalid year: {s} ({any})\n", .{ args[2], err });
        std.process.exit(1);
    };

    const dayInput = std.fmt.parseInt(u8, args[1], 10) catch |err| {
        std.debug.print("Invalid day: {s} ({any})\n", .{ args[3], err });
        std.process.exit(1);
    };

    var days_map = try createDaysMap(allocator);
    defer days_map.deinit();

    const dayInstance = days_map.get(DayKey{ .year = yearInput, .day = dayInput }) orelse {
        std.debug.print("Day {d} not found for year {d}\n", .{ dayInput, yearInput });
        std.process.exit(1);
    };

    const part_one_result = try dayInstance.partOne(allocator);
    defer allocator.free(part_one_result);
    std.debug.print("{s}\n", .{part_one_result});

    const part_two_result = try dayInstance.partTwo(allocator);
    defer allocator.free(part_two_result);
    std.debug.print("{s}\n", .{part_two_result});
}
