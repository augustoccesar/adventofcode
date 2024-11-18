const std = @import("std");

const MAX_FILE_SIZE: usize = 1024 * 1000;

pub fn readLines(allocator: std.mem.Allocator, input_path: []u8) ![][]const u8 {
    const input = readFileAlloc(allocator, input_path);

    var list = std.ArrayList([]const u8).init(allocator);
    defer list.deinit();

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        try list.append(line);
    }

    return try list.toOwnedSlice();
}

pub fn linesIterator(allocator: std.mem.Allocator, input_path: []u8) std.mem.SplitIterator(u8, .scalar) {
    const input = readFileAlloc(allocator, input_path);

    return std.mem.splitScalar(u8, input, '\n');
}

pub fn readString(allocator: std.mem.Allocator, input_path: []u8) []const u8 {
    return readFileAlloc(allocator, input_path);
}

fn readFileAlloc(allocator: std.mem.Allocator, input_path: []u8) []const u8 {
    return std.fs.cwd().readFileAlloc(allocator, input_path, MAX_FILE_SIZE) catch |err| {
        std.io.getStdOut().writer().print("Failed to read input on {s}. Cause: {any}", .{ input_path, err }) catch {
            std.process.exit(1);
        };

        std.process.exit(1);
    };
}
