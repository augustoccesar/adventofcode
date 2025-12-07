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
pub struct CSharp;

impl ManagedLanguage for CSharp {
    fn run(&self, year: u16, day: u8) {
        process::Command::new("dotnet")
            .args([
                "run",
                "--",
                "run",
                "--year",
                &format!("{year}"),
                "--day",
                &format!("{day}"),
            ])
            .current_dir(crate::base_path().join("csharp"))
            .stderr(Stdio::null())
            .status()
            .unwrap();
    }

    fn available_days(&self) -> HashMap<u16, Vec<u8>> {
        let output = process::Command::new("dotnet")
            .args(["run", "--", "days"])
            .current_dir(crate::base_path().join("csharp"))
            .stderr(Stdio::null())
            .output()
            .unwrap();

        parse_year_available_days(String::from_utf8_lossy(&output.stdout).as_ref())
    }

    fn prepare_day(&self, year: u16, day: u8) -> PathBuf {
        let day_file_path = crate::base_path().join(format!("csharp/y{year}/Day{day:0>2}.cs"));

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

            process::Command::new("dotnet")
                .arg("format")
                .current_dir(crate::base_path().join("csharp"))
                .status()
                .unwrap();
        }

        day_file_path
    }

    fn path_to_day(&self, year: u16, day: u8) -> PathBuf {
        PathBuf::from_str(&format!("/csharp/y{year}/Day{day:0>2}.cs")).unwrap()
    }
}

const DAY_TEMPLATE: &str = r#"namespace aoc.y$year;

[RunnableDay(year: $year, day: $day)]
class Day$padded_day : Day
{
    public override string PartOne()
    {
        return "-";
    }

    public override string PartTwo()
    {
        return "-";
    }
}

"#;
