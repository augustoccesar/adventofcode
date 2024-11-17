const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut().writer();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try stdout.writeAll("Missing day to run.\n");
        std.process.exit(1);
    }

    var input_name = try allocator.dupe(u8, "input");
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--input")) {
            if (i + 1 < args.len) {
                input_name = try allocator.dupe(u8, args[i + 1]);
            } else {
                try stdout.writeAll("Missing input name.\n");
                std.process.exit(1);
            }
            i += 1;
        }
    }

    const day_u8 = try std.fmt.parseInt(u8, args[1], 10);

    switch (day_u8) {
        0 => try run(allocator, stdout, @import("./day00/task.zig").task, &input_name),
        1 => try run(allocator, stdout, @import("./day01/task.zig").task, &input_name),
        2 => try run(allocator, stdout, @import("./day02/task.zig").task, &input_name),
        3 => try run(allocator, stdout, @import("./day03/task.zig").task, &input_name),
        4 => try run(allocator, stdout, @import("./day04/task.zig").task, &input_name),
        5 => try run(allocator, stdout, @import("./day05/task.zig").task, &input_name),
        6 => try run(allocator, stdout, @import("./day06/task.zig").task, &input_name),
        7 => try run(allocator, stdout, @import("./day07/task.zig").task, &input_name),
        8 => try run(allocator, stdout, @import("./day08/task.zig").task, &input_name),
        9 => try run(allocator, stdout, @import("./day09/task.zig").task, &input_name),
        10 => try run(allocator, stdout, @import("./day10/task.zig").task, &input_name),
        //SETUP:target_tasks
        else => {
            std.debug.print("Unknown day {d}\n", .{day_u8});
            std.process.exit(1);
        },
    }
}

fn run(allocator: std.mem.Allocator, stdout: anytype, task: anytype, input_name: *const []u8) !void {
    const path = try std.fmt.allocPrint(
        allocator,
        "./inputs/day{d:0>2}_{s}.txt",
        .{
            task.day,
            input_name.*,
        },
    );
    const input = try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1000);

    const res_part_one = try task.p1(allocator, input);
    const res_part_two = try task.p2(allocator, input);

    try stdout.print("Part one: {s}\n", .{res_part_one});
    try stdout.print("Part two: {s}\n", .{res_part_two});
}
