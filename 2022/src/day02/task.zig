const std = @import("std");

const helpers = @import("../helpers.zig");

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

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

    fn from_u8(value: u8) Result {
        return switch (value) {
            'X' => Result.loss,
            'Y' => Result.tie,
            'Z' => Result.win,
            else => @panic("invalid value for Result"),
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
            else => @panic("invalid value for Play"),
        };
    }

    pub fn win_against(self: Play) Play {
        return switch (self) {
            Play.rock => Play.scissors,
            Play.paper => Play.rock,
            Play.scissors => Play.paper,
        };
    }

    pub fn loses_against(self: Play) Play {
        return switch (self) {
            Play.rock => Play.paper,
            Play.paper => Play.scissors,
            Play.scissors => Play.rock,
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

    pub fn matching_play(opponent_play: Play, my_result: Result) Play {
        return switch (my_result) {
            .loss => opponent_play.win_against(),
            .tie => opponent_play,
            .win => opponent_play.loses_against(),
        };
    }
};

fn partOne(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    var total: u64 = 0;
    var iterator = helpers.input.linesIterator(allocator, input_path);
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

fn partTwo(allocator: std.mem.Allocator, input_path: []u8) TaskError![]const u8 {
    var total: u64 = 0;
    var iterator = helpers.input.linesIterator(allocator, input_path);
    while (iterator.next()) |line| {
        var items = std.mem.splitAny(u8, line, " ");
        const opponent_play_str = items.next() orelse @panic("failed to decode opponent play");
        const expected_result_str = items.next() orelse @panic("failed to decode my play");

        const opponent_play = Play.from_u8(opponent_play_str[0]);
        const expected_result = Result.from_u8(expected_result_str[0]);

        const my_play = Play.matching_play(opponent_play, expected_result);

        total += expected_result.points();
        total += my_play.points();
    }

    return std.fmt.allocPrint(allocator, "{d}", .{total});
}

pub const task = Task{
    .day = 2,
    .p1 = partOne,
    .p2 = partTwo,
};
