const std = @import("std");

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const linesIterator = @import("../input.zig").linesIterator;

const InstructionType = enum { noop, addx };

const Instruction = struct {
    ty: InstructionType,
    value: ?i64 = null,
    started_at: ?i64 = null,

    fn from_str(str: *const []const u8) !Instruction {
        var parts = std.mem.splitAny(u8, str.*, " ");
        const instruction_cmd = parts.next().?;

        if (std.mem.eql(u8, instruction_cmd, "noop")) {
            return .{
                .ty = InstructionType.noop,
            };
        } else if (std.mem.eql(u8, instruction_cmd, "addx")) {
            const value = try std.fmt.parseInt(i64, parts.next().?, 10);

            return .{
                .ty = InstructionType.addx,
                .value = value,
            };
        } else {
            @panic("Received unsupported instruction");
        }
    }

    fn start(self: *Instruction, cycle: i64) void {
        self.*.started_at = cycle;
    }

    fn is_finished(self: Instruction, cycle: i64) bool {
        return switch (self.ty) {
            .noop => (cycle - self.started_at.?) == 1,
            .addx => (cycle - self.started_at.?) == 2,
        };
    }
};

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var result: i64 = 0;

    var register_x: i64 = 1;
    var cycle: i64 = 0;

    var lines_iter = linesIterator(input);
    while (lines_iter.next()) |line| {
        var instruction = try Instruction.from_str(&line);
        instruction.start(cycle);

        while (!instruction.is_finished(cycle)) {
            cycle += 1;
            if (cycle == 20 or cycle == 60 or cycle == 100 or cycle == 140 or cycle == 180 or cycle == 220) {
                result += (cycle * register_x);
            }
        }

        switch (instruction.ty) {
            .noop => {},
            .addx => {
                register_x += instruction.value.?;
            },
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{result});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = allocator;
    _ = input;

    return "-";
}

pub const task = Task{
    .day = 10,
    .p1 = partOne,
    .p2 = partTwo,
};