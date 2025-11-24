const std = @import("std");

const MAX_FILE_SIZE: usize = 1024 * 1000;

fn input_path(allocator: std.mem.Allocator, year: u16, day: u8, name: []const u8) ![]const u8 {
    const path = if (name.len == 0)
        try std.fmt.allocPrint(allocator, "../inputs/{d}_{d:0>2}.txt", .{ year, day })
    else
        try std.fmt.allocPrint(allocator, "../inputs/{d}_{d:0>2}_{s}.txt", .{ year, day, name });

    return path;
}

pub fn readLines(allocator: std.mem.Allocator, year: u16, day: u8, name: []const u8) ![][]const u8 {
    const input = try readFileAlloc(allocator, year, day, name);
    defer allocator.free(input);

    var list = std.ArrayList([]const u8).init(allocator);
    defer list.deinit();

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        const line_copy = try allocator.dupe(u8, line);
        try list.append(line_copy);
    }

    return try list.toOwnedSlice();
}

pub const LinesIterator = struct {
    input: []const u8,
    iterator: std.mem.SplitIterator(u8, .scalar),
    allocator: std.mem.Allocator,

    pub fn next(self: *LinesIterator) ?[]const u8 {
        return self.iterator.next();
    }

    pub fn deinit(self: *LinesIterator) void {
        self.allocator.free(self.input);
    }
};

pub fn linesIterator(allocator: std.mem.Allocator, year: u16, day: u8, name: []const u8) !LinesIterator {
    const input = try readFileAlloc(allocator, year, day, name);

    return LinesIterator{
        .input = input,
        .iterator = std.mem.splitScalar(u8, input, '\n'),
        .allocator = allocator,
    };
}

pub fn readString(allocator: std.mem.Allocator, year: u16, day: u8, name: []const u8) ![]const u8 {
    return try readFileAlloc(allocator, year, day, name);
}

fn readFileAlloc(allocator: std.mem.Allocator, year: u16, day: u8, name: []const u8) ![]const u8 {
    const path = try input_path(allocator, year, day, name);
    defer allocator.free(path);

    return std.fs.cwd().readFileAlloc(allocator, path, MAX_FILE_SIZE) catch |err| {
        std.debug.print("Failed to read input on {s}. Cause: {any}", .{ path, err });
        std.process.exit(1);
    };
}
