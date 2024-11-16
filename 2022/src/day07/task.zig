const std = @import("std");
const print = std.debug.print;

const Task = @import("../task.zig").Task;
const TaskError = @import("../task.zig").TaskError;

const linesIterator = @import("../input.zig").linesIterator;
const readLines = @import("../input.zig").readLines;

const File = struct {
    name: []const u8,
    size: u64,

    fn init(allocator: std.mem.Allocator, name: []const u8, size: u64) !*File {
        const file = try allocator.create(File);
        file.* = File{ .name = name, .size = size };

        return file;
    }
};

const Folder = struct {
    name: []const u8,
    subfolders: std.ArrayList(*Folder),
    files: std.ArrayList(*File),

    fn init(allocator: std.mem.Allocator, name: []const u8) !*Folder {
        const folder = try allocator.create(Folder);
        folder.* = Folder{
            .name = name,
            .subfolders = std.ArrayList(*Folder).init(allocator),
            .files = std.ArrayList(*File).init(allocator),
        };

        return folder;
    }

    fn size(self: *Folder) u64 {
        var total: u64 = 0;

        for (self.files.items) |file| {
            total += file.size;
        }

        for (self.subfolders.items) |folder| {
            total += folder.size();
        }

        return total;
    }

    fn lookup_lte(self: *Folder, allocator: std.mem.Allocator, lookup_size: u64) ![]std.meta.Tuple(&.{ *Folder, u64 }) {
        var res = std.ArrayList(std.meta.Tuple(&.{ *Folder, u64 })).init(allocator);
        const total = self.size();

        if (total <= lookup_size) {
            try res.append(.{ self, total });
        }

        for (self.subfolders.items) |subfolder| {
            try res.appendSlice(try subfolder.lookup_lte(allocator, lookup_size));
        }

        return res.toOwnedSlice();
    }

    fn lookup_gte(self: *Folder, allocator: std.mem.Allocator, lookup_size: u64) ![]std.meta.Tuple(&.{ *Folder, u64 }) {
        var res = std.ArrayList(std.meta.Tuple(&.{ *Folder, u64 })).init(allocator);
        const total = self.size();

        if (total >= lookup_size) {
            try res.append(.{ self, total });
        }

        for (self.subfolders.items) |subfolder| {
            try res.appendSlice(try subfolder.lookup_gte(allocator, lookup_size));
        }

        return res.toOwnedSlice();
    }
};

fn is_cd(line: []const u8) bool {
    return std.mem.eql(u8, line[0..4], "$ cd");
}

fn is_ls(line: []const u8) bool {
    return std.mem.eql(u8, line[0..4], "$ ls");
}

fn is_command(line: []const u8) bool {
    return is_cd(line) or is_ls(line);
}

fn parse_filesystem(allocator: std.mem.Allocator, lines: *const [][]const u8) !*Folder {
    var root: ?*Folder = null;
    var navigation_stack = std.ArrayList(*Folder).init(allocator);

    var i: usize = 0;
    while (i < lines.len) {
        const line = lines.*[i];

        if (is_cd(line)) {
            var parts = std.mem.splitAny(u8, line, " ");
            _ = parts.next(); // Dollar sign
            _ = parts.next(); // Command itself
            const folder_name = parts.next().?;

            if (std.mem.eql(u8, folder_name, "..")) {
                _ = navigation_stack.pop();
            } else if (std.mem.eql(u8, folder_name, "/")) {
                if (root == null) {
                    const folder = try Folder.init(allocator, folder_name);
                    root = folder;
                    try navigation_stack.append(folder);
                } else {
                    navigation_stack.clearRetainingCapacity();
                    try navigation_stack.append(root.?);
                }
            } else {
                const current_folder = navigation_stack.getLast();

                var existing_folder: ?*Folder = null;
                for (current_folder.subfolders.items) |subfolder| {
                    if (std.mem.eql(u8, subfolder.name, folder_name)) {
                        existing_folder = subfolder;
                    }
                }

                if (existing_folder) |f| {
                    try navigation_stack.append(f);
                } else {
                    const folder = try Folder.init(allocator, folder_name);

                    try current_folder.*.subfolders.append(folder);
                    try navigation_stack.append(folder);
                }
            }

            i += 1;
        } else if (is_ls(line)) {
            const current_folder = navigation_stack.getLast();

            var j: usize = 1;
            while (j + i < lines.len and !is_command(lines.*[j + i])) : (j += 1) {
                var parts = std.mem.splitAny(u8, lines.*[j + i], " ");
                const first = parts.next().?;

                if (std.mem.eql(u8, first, "dir")) {
                    const folder_name = parts.next().?;

                    var already_exist = false;
                    for (current_folder.subfolders.items) |subfolder| {
                        if (std.mem.eql(u8, subfolder.name, folder_name)) {
                            already_exist = true;
                        }
                    }

                    if (!already_exist) {
                        const folder = try Folder.init(allocator, folder_name);

                        try current_folder.*.subfolders.append(folder);
                    }
                } else {
                    const file_size = try std.fmt.parseInt(u64, first, 10);
                    const file_name = parts.next().?;

                    const file = try File.init(allocator, file_name, file_size);

                    try current_folder.*.files.append(file);
                }
            }

            i += j;
        } else {
            unreachable;
        }
    }

    return root.?;
}

fn partOne(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    const lines = try readLines(allocator, input);
    defer allocator.free(lines);

    var root = try parse_filesystem(allocator, &lines);

    var result: u64 = 0;
    for (try root.lookup_lte(allocator, 100000)) |res| {
        result += res[1];
    }

    return std.fmt.allocPrint(allocator, "{d}", .{result});
}

fn partTwo(allocator: std.mem.Allocator, input: []u8) TaskError![]const u8 {
    const lines = try readLines(allocator, input);
    defer allocator.free(lines);

    const disk_space: u64 = 70_000_000;

    var root = try parse_filesystem(allocator, &lines);
    const unused_space: u64 = disk_space - root.size();
    const needed_space: u64 = 30_000_000 - unused_space;

    var min: u64 = std.math.maxInt(u64);
    for (try root.lookup_gte(allocator, needed_space)) |option| {
        if (option[1] < min) {
            min = option[1];
        }
    }

    return std.fmt.allocPrint(allocator, "{d}", .{min});
}

pub const task = Task{
    .day = 7,
    .p1 = partOne,
    .p2 = partTwo,
};
