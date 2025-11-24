use std::{
    fs::{self, OpenOptions},
    io::Write,
    process,
};

pub fn run(year: u16, day: u8) {
    process::Command::new("zig")
        .args([
            "build",
            "run",
            "--",
            "run",
            &format!("{}", year),
            &format!("{}", day),
        ])
        .current_dir(crate::base_path().join("zig"))
        .status()
        .unwrap();
}

pub fn prepare_day(year: u16, day: u8) {
    let day_file_path = crate::base_path().join(&format!("zig/src/y{year}/d{day:0>2}.zig"));

    if !day_file_path.exists() {
        day_file_path
            .parent()
            .map(|path| fs::create_dir_all(path).unwrap());

        let mut day_file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(day_file_path)
            .unwrap();

        day_file
            .write_all(
                DAY_TEMPLATE
                    .replace("$day", &format!("{}", day))
                    .replace("$year", &format!("{}", year))
                    .as_bytes(),
            )
            .unwrap();

        let mut main_file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(crate::base_path().join("zig/src/main.zig"))
            .unwrap();

        crate::file::insert_codegen(
            &mut main_file,
            "import_day",
            &format!("const y{year}d{day:0>2} = @import(\"y{year}/d{day:0>2}.zig\");\n"),
        );

        crate::file::insert_codegen(
            &mut main_file,
            "register_day",
            &format!("y{year}d{day:0>2}.getDay(),\n"),
        );
    }
}

const DAY_TEMPLATE: &str = r#"const std = @import("std");

const Day = @import("../day.zig").Day;

pub fn getDay() Day {
    return Day{
        .year = $year,
        .day = $day,
        .part_one_fn = partOne,
        .part_two_fn = partTwo,
    };
}

fn partOne(allocator: std.mem.Allocator) ![]const u8 {
    return try allocator.dupe(u8, "-");
}

fn partTwo(allocator: std.mem.Allocator) ![]const u8 {
    return try allocator.dupe(u8, "-");
}

"#;
