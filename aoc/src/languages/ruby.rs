use std::{
    collections::HashMap,
    fs::{self, OpenOptions},
    io::Write,
    path::PathBuf,
    process::{self, Stdio},
    str::FromStr,
};

use crate::languages::{ManagedLanguage, parse_year_available_days};

#[derive(Default)]
pub struct Ruby;

impl ManagedLanguage for Ruby {
    fn run(&self, year: u16, day: u8) -> String {
        let stdout = process::Command::new("bundle")
            .args([
                "exec",
                "ruby",
                "main.rb",
                "run",
                &format!("{}", year),
                &format!("{}", day),
            ])
            .current_dir(crate::base_path().join("ruby"))
            .output()
            .unwrap()
            .stdout;

        String::from_utf8(stdout).expect("stdout should be valid UTF-8")
    }

    fn available_days(&self) -> HashMap<u16, Vec<u8>> {
        let output = process::Command::new("bundle")
            .args(["exec", "ruby", "main.rb", "days"])
            .current_dir(crate::base_path().join("ruby"))
            .stderr(Stdio::null())
            .output()
            .unwrap();

        parse_year_available_days(String::from_utf8_lossy(&output.stdout).as_ref())
    }

    fn prepare_day(&self, year: u16, day: u8) -> PathBuf {
        let day_file_path = crate::base_path().join(format!("ruby/y{year}/d{day:0>2}.rb"));

        if !day_file_path.exists() {
            if let Some(path) = day_file_path.parent() {
                fs::create_dir_all(path).unwrap();
            }

            let mut day_file = OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&day_file_path)
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

        day_file_path
    }

    fn path_to_day(&self, year: u16, day: u8) -> PathBuf {
        PathBuf::from_str(&format!("/ruby/y{year}/d{day:0>2}.rb")).unwrap()
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
