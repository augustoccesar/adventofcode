const std = @import("std");

const Task = @import("./task.zig").Task;

pub fn get_task(day: u8) ?Task {
    return switch (day) {
        0 => @import("./day00/task.zig").task,
        1 => @import("./day01/task.zig").task,
        2 => @import("./day02/task.zig").task,
        3 => @import("./day03/task.zig").task,
        4 => @import("./day04/task.zig").task,
        5 => @import("./day05/task.zig").task,
        6 => @import("./day06/task.zig").task,
        7 => @import("./day07/task.zig").task,
        8 => @import("./day08/task.zig").task,
        9 => @import("./day09/task.zig").task,
        10 => @import("./day10/task.zig").task,
        11 => @import("./day11/task.zig").task,
        else => null,
    };
}
