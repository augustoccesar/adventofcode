use std::{
    fs::{self, OpenOptions},
    io::Write,
    process::{self},
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
        .current_dir(crate::base_path().join("cpp"))
        .status()
        .unwrap();
}

pub fn prepare_day(year: u16, day: u8) {
    let day_file_path = crate::base_path().join(format!("cpp/src/y{year}/d{day:0>2}.cpp"));

    if !day_file_path.exists() {
        if let Some(path) = day_file_path.parent() {
            fs::create_dir_all(path).unwrap()
        }

        let mut day_file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(day_file_path)
            .unwrap();

        day_file
            .write_all(
                DAY_TEMPLATE
                    .replace("$padded_day", &format!("{day:0>2}"))
                    .replace("$day", &format!("{}", day))
                    .replace("$year", &format!("{}", year))
                    .as_bytes(),
            )
            .unwrap();

        let mut build_file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(crate::base_path().join("cpp/build.zig"))
            .unwrap();

        crate::file::insert_codegen(
            &mut build_file,
            "register_day",
            &format!("\"src/y{year}/d{day:0>2}.cpp\"\n"),
        );

        process::Command::new("zig")
            .args(["fmt", "."])
            .current_dir(crate::base_path().join("cpp"))
            .status()
            .unwrap();
    }
}

const DAY_TEMPLATE: &str = r#"#include "../Day.hpp"

class Day$padded_day : public Day
{
public:
  Day$padded_day(int year, int day) : Day(year, day) {}

  std::string partOne() override
  {
    return "-";
  }

  std::string partTwo() override
  {
    return "-";
  }
};

REGISTER_DAY($year, $day, Day$padded_day);

"#;
