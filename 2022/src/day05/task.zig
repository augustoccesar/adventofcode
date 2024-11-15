const std = @import("std");
const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;
const readLines = @import("../input.zig").readLines;

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    const lines = try readLines(allocator, input);

    var end_of_stacks_idx: usize = 0;
    for (0..lines.len) |i| {
        if (std.mem.eql(u8, lines[i][0..3], " 1 ")) {
            end_of_stacks_idx = i;
            break;
        }
    }

    const stacks_count = (lines[0].len / 4) + 1;
    var stacks = std.ArrayList(std.ArrayList(u8)).init(allocator);
    for (0..stacks_count) |_| {
        try stacks.append(std.ArrayList(u8).init(allocator));
    }

    var i = end_of_stacks_idx;
    while (i > 0) {
        i -= 1;

        var j: usize = 1;
        while (j < lines[i].len) : (j += 4) {
            const stack_idx = j / 4;
            const item = lines[i][j];

            if (item != 32) {
                try stacks.items[stack_idx].append(item);
            }
        }
    }

    for ((end_of_stacks_idx + 2)..lines.len) |j| {
        const procedure = lines[j];
        var parts = std.mem.splitAny(u8, procedure, " ");

        var amount: u8 = 0;
        var from_stack_idx: usize = 0;
        var to_stack_idx: usize = 0;

        var k: usize = 0;
        while (parts.next()) |part| {
            if (k == 1) {
                amount = try std.fmt.parseInt(u8, part, 10);
            } else if (k == 3) {
                from_stack_idx = (try std.fmt.parseInt(usize, part, 10)) - 1;
            } else if (k == 5) {
                to_stack_idx = (try std.fmt.parseInt(usize, part, 10)) - 1;
            }

            k += 1;
        }

        for (0..amount) |_| {
            try stacks.items[to_stack_idx].append(stacks.items[from_stack_idx].pop());
        }
    }

    var result = std.ArrayList(u8).init(allocator);
    for (stacks.items) |stack| {
        try result.append(stack.items[stack.items.len - 1]);
    }

    return std.fmt.allocPrint(allocator, "{s}", .{result.items});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    const lines = try readLines(allocator, input);

    var end_of_stacks_idx: usize = 0;
    for (0..lines.len) |i| {
        if (std.mem.eql(u8, lines[i][0..3], " 1 ")) {
            end_of_stacks_idx = i;
            break;
        }
    }

    const stacks_count = (lines[0].len / 4) + 1;
    var stacks = std.ArrayList(std.ArrayList(u8)).init(allocator);
    for (0..stacks_count) |_| {
        try stacks.append(std.ArrayList(u8).init(allocator));
    }

    var i = end_of_stacks_idx;
    while (i > 0) {
        i -= 1;

        var j: usize = 1;
        while (j < lines[i].len) : (j += 4) {
            const stack_idx = j / 4;
            const item = lines[i][j];

            if (item != 32) {
                try stacks.items[stack_idx].append(item);
            }
        }
    }

    for ((end_of_stacks_idx + 2)..lines.len) |j| {
        const procedure = lines[j];
        var parts = std.mem.splitAny(u8, procedure, " ");

        var amount: u8 = 0;
        var from_stack_idx: usize = 0;
        var to_stack_idx: usize = 0;

        var k: usize = 0;
        while (parts.next()) |part| {
            if (k == 1) {
                amount = try std.fmt.parseInt(u8, part, 10);
            } else if (k == 3) {
                from_stack_idx = (try std.fmt.parseInt(usize, part, 10)) - 1;
            } else if (k == 5) {
                to_stack_idx = (try std.fmt.parseInt(usize, part, 10)) - 1;
            }

            k += 1;
        }

        var crates = std.ArrayList(u8).init(allocator);
        for (0..amount) |_| {
            try crates.append(stacks.items[from_stack_idx].pop());
        }

        var l: usize = crates.items.len;
        while (l > 0) {
            l -= 1;

            try stacks.items[to_stack_idx].append(crates.items[l]);
        }
    }

    var result = std.ArrayList(u8).init(allocator);
    for (stacks.items) |stack| {
        try result.append(stack.items[stack.items.len - 1]);
    }

    return std.fmt.allocPrint(allocator, "{s}", .{result.items});
}

pub const task = Task{
    .day = 5,
    .p1 = partOne,
    .p2 = partTwo,
};
