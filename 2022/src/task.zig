const std = @import("std");

pub const TaskError = std.fmt.ParseIntError || std.fmt.AllocPrintError;

pub const Task = struct {
    day: u8,
    p1: *const fn (std.mem.Allocator, []u8) TaskError![]const u8,
    p2: *const fn (std.mem.Allocator, []u8) TaskError![]const u8,
};
