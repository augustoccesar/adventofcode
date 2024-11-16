const std = @import("std");

pub fn readLines(allocator: std.mem.Allocator, input: []u8) !std.ArrayList([]const u8) {
    var list = std.ArrayList([]const u8).init(allocator);

    var lines = std.mem.splitAny(u8, input, "\n");
    while (lines.next()) |line| {
        try list.append(line);
    }

    return list;
}

pub fn linesIterator(input: []u8) std.mem.SplitIterator(u8, .any) {
    return std.mem.splitAny(u8, input, "\n");
}
