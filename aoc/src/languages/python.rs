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
pub struct Python;

impl ManagedLanguage for Python {
    fn run(&self, year: u16, day: u8) -> String {
        let stdout = process::Command::new("uv")
            .args([
                "-q",
                "run",
                "main.py",
                "run",
                &format!("{}", year),
                &format!("{}", day),
            ])
            .current_dir(crate::base_path().join("python"))
            .output()
            .unwrap()
            .stdout;

        String::from_utf8(stdout).expect("stdout should be valid UTF-8")
    }

    fn available_days(&self) -> HashMap<u16, Vec<u8>> {
        let output = process::Command::new("uv")
            .args(["-q", "run", "main.py", "days"])
            .current_dir(crate::base_path().join("python"))
            .stderr(Stdio::null())
            .output()
            .unwrap();

        parse_year_available_days(String::from_utf8_lossy(&output.stdout).as_ref())
    }

    fn prepare_day(&self, year: u16, day: u8) -> PathBuf {
        let day_file_path = crate::base_path().join(format!("python/y{year}/d{day:0>2}.py"));

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
                        .replace("$day", &format!("{}", day))
                        .replace("$year", &format!("{}", year))
                        .as_bytes(),
                )
                .unwrap();

            let mut main_file = OpenOptions::new()
                .read(true)
                .write(true)
                .open(crate::base_path().join("python/main.py"))
                .unwrap();

            crate::file::insert_codegen(
                &mut main_file,
                "import_day",
                &format!("import y{year}.d{day:0>2}\n"),
            );

            process::Command::new("uv")
                .args(["-q", "format"])
                .current_dir(crate::base_path().join("python"))
                .status()
                .unwrap();
        }

        day_file_path
    }

    fn path_to_day(&self, year: u16, day: u8) -> PathBuf {
        PathBuf::from_str(&format!("/python/y{year}/d{day:0>2}.py")).unwrap()
    }
}

const DAY_TEMPLATE: &str = r#"from day import Day, register_day

@register_day($year, $day)
class Day$padded_day(Day):
    def part_one(self) -> str:
        return "-"

    def part_two(self) -> str:
        return "-"
"#;
