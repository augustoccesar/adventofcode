const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const stdout = std.io.getStdOut().writer();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 2 or args.len < 2) {
        try stdout.writeAll("Invalid amount of arguments.\n");
        std.process.exit(1);
    }

    const day_u8 = try std.fmt.parseInt(u8, args[1], 10);

    switch (day_u8) {
        0 => try run(allocator, stdout, @import("./day00/task.zig").task),
        1 => try run(allocator, stdout, @import("./day01/task.zig").task),
        2 => try run(allocator, stdout, @import("./day02/task.zig").task),
        3 => try run(allocator, stdout, @import("./day03/task.zig").task),
        4 => try run(allocator, stdout, @import("./day04/task.zig").task),
		//SETUP:target_tasks
        else => {
            std.debug.print("Unknown day {d}\n", .{day_u8});
            std.process.exit(1);
        },
    }
}

fn run(allocator: std.mem.Allocator, stdout: anytype, task: anytype) !void {
    const path = try std.fmt.allocPrint(allocator, "./inputs/day{d:0>2}_input.txt", .{task.day});
    const input = try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1000);

    const res_part_one = try task.p1(allocator, input);
    const res_part_two = try task.p2(allocator, input);

    try stdout.print("Part one: {s}\n", .{res_part_one});
    try stdout.print("Part two: {s}\n", .{res_part_two});
}
