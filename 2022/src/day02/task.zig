const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const linesIterator = @import("../input.zig").linesIterator;

const Result = enum {
    loss,
    tie,
    win,

    fn points(self: Result) u64 {
        return switch (self) {
            .loss => 0,
            .tie => 3,
            .win => 6,
        };
    }
};
const Play = enum {
    rock,
    paper,
    scissors,

    pub fn points(self: Play) u64 {
        switch (self) {
            .rock => return 1,
            .paper => return 2,
            .scissors => return 3,
        }
    }

    pub fn from_u8(value: u8) Play {
        return switch (value) {
            'A', 'X' => Play.rock,
            'B', 'Y' => Play.paper,
            'C', 'Z' => Play.scissors,
            else => @panic("invalid value for play"),
        };
    }

    pub fn result(left: Play, right: Play) Result {
        if (left == right) return Result.tie;
        if (left == Play.rock and right == Play.scissors) return Result.win;
        if (left == Play.rock and right == Play.paper) return Result.loss;
        if (left == Play.paper and right == Play.rock) return Result.win;
        if (left == Play.paper and right == Play.scissors) return Result.loss;
        if (left == Play.scissors and right == Play.rock) return Result.loss;
        if (left == Play.scissors and right == Play.paper) return Result.win;

        unreachable;
    }
};

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    var total: u64 = 0;
    var iterator = linesIterator(input);
    while (iterator.next()) |line| {
        var items = std.mem.splitAny(u8, line, " ");
        const opponent_play_str = items.next() orelse @panic("failed to decode opponent play");
        const my_play_str = items.next() orelse @panic("failed to decode my play");

        const opponent_play = Play.from_u8(opponent_play_str[0]);
        const my_play = Play.from_u8(my_play_str[0]);

        const result = Play.result(my_play, opponent_play);

        total += result.points();
        total += my_play.points();
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    _ = allocator;
    _ = input;

    return "-";
}

pub const task = Task{
    .day = 2,
    .p1 = partOne,
    .p2 = partTwo,
};

fn countPoints(opponent_play: u8, my_play: u8) u64 {
    if (opponent_play == my_play) {}

    // if (opponent_play == 'A' or opponent_play == 'X') {}
}

// fn point_from_play(play: u8) {
//     if (play == 'A' || play == '')
// }
