use std::{
    fs::{self, OpenOptions},
    io::Write,
    process::{self},
};

pub fn run(year: u16, day: u8) {
    process::Command::new("bundle")
        .args([
            "exec",
            "ruby",
            "main.rb",
            "run",
            &format!("{}", year),
            &format!("{}", day),
        ])
        .current_dir(crate::base_path().join("ruby"))
        .status()
        .unwrap();
}

pub fn prepare_day(year: u16, day: u8) {
    let day_file_path = crate::base_path().join(format!("ruby/y{year}/d{day:0>2}.rb"));

    if !day_file_path.exists() {
        if let Some(path) = day_file_path.parent() {
            fs::create_dir_all(path).unwrap();
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
                    .as_bytes(),
            )
            .unwrap();

        process::Command::new("bundle")
            .args(["exec", "rubocop", "-a", "--format", "quiet"])
            .current_dir(crate::base_path().join("ruby"))
            .status()
            .unwrap();
    }
}

const DAY_TEMPLATE: &str = r#"# frozen_string_literal: true

require_relative '../day'

class Day$padded_day < Day
  def part_one
    '-'
  end

  def part_two
    '-'
  end
end
"#;
