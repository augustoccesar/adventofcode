const std = @import("std");

pub fn readLines(allocator: std.mem.Allocator, input: []u8) [][]u8 {
    var list = std.ArrayList([]u8).init(allocator);
    defer list.deinit();

    const lines = std.mem.splitAny(u8, input, "\n");
    return std.mem.bytesAsSlice([][]const u8, lines);
}

pub fn linesIterator(input: []u8) std.mem.SplitIterator(u8, .any) {
    return std.mem.splitAny(u8, input, "\n");
}
