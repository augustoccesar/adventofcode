const std = @import("std");

const days = @import("./days.zig");

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

    if (days.get_task(day_u8)) |task| {
        try run(allocator, stdout, task, &input_name);
    } else {
        std.debug.print("Unknown day {d}\n", .{day_u8});
        std.process.exit(1);
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

    const input_real_path = try std.fs.realpathAlloc(allocator, path);

    const res_part_one = try task.p1(allocator, input_real_path);
    const res_part_two = try task.p2(allocator, input_real_path);

    try stdout.print("Part one: {s}\n", .{res_part_one});
    try stdout.print("Part two: {s}\n", .{res_part_two});
}
