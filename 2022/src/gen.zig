const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var args = std.process.args();
    _ = args.next();

    const filename = args.next().?;

    var file = try std.fs.cwd().createFile(filename, .{});
    defer file.close();
    var bw = std.io.bufferedWriter(file.writer());
    const writer = bw.writer();

    try writer.writeAll(
        \\const std = @import("std");
        \\
        \\const Task = @import("./task.zig").Task;
        \\
        \\pub fn get_task(day: u8) ?Task {
        \\    return switch (day) {
        \\
    );

    var dir_src = try std.fs.cwd().openDir("src", .{ .iterate = true });
    var dir_src_iter = dir_src.iterate();

    var days = std.ArrayList(u8).init(allocator);
    while (try dir_src_iter.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.directory and std.mem.eql(u8, entry.name[0..3], "day")) {
            var day_iter = std.mem.splitBackwardsSequence(u8, entry.name, "day");
            const day = try std.fmt.parseInt(u8, day_iter.next().?, 10);
            try days.append(day);
        }
    }

    std.mem.sort(u8, days.items, {}, std.sort.asc(u8));
    for (days.items) |day| {
        const day_str = try std.fmt.allocPrint(allocator, "{d}", .{day});

        const content_day = try std.fmt.allocPrint(
            allocator,
            "        {d} => @import(\"./day{s:0>2}/task.zig\").task,\n",
            .{ day, day_str },
        );
        try writer.writeAll(content_day);
    }

    try writer.writeAll(
        \\        else => null,
        \\    };
        \\}
        \\
    );

    try bw.flush();
}
