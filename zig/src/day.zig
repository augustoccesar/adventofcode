const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Day = struct {
    year: u16,
    day: u8,
    part_one_fn: *const fn (allocator: Allocator) anyerror![]const u8,
    part_two_fn: *const fn (allocator: Allocator) anyerror![]const u8,

    pub fn partOne(self: Day, allocator: Allocator) ![]const u8 {
        return try self.part_one_fn(allocator);
    }

    pub fn partTwo(self: Day, allocator: Allocator) ![]const u8 {
        return try self.part_two_fn(allocator);
    }
};
