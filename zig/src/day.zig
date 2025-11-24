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

    pub fn readInput(self: Day, allocator: Allocator, name: []const u8) ![]const u8 {
        const path = if (name.len == 0)
            try std.fmt.allocPrint(allocator, "../inputs/{d}_{d:0>2}.txt", .{ self.year, self.day })
        else
            try std.fmt.allocPrint(allocator, "../inputs/{d}_{d:0>2}_{s}.txt", .{ self.year, self.day, name });
        defer allocator.free(path);

        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            std.debug.print("Error opening file {s}: {any}\n", .{ path, err });
            return err;
        };
        defer file.close();

        const content = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
        return content;
    }

    pub fn readDefaultInput(self: Day, allocator: Allocator) ![]const u8 {
        return try self.readInput(allocator, "");
    }
};
