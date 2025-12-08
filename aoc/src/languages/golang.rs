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
pub struct Golang;

impl ManagedLanguage for Golang {
    fn run(&self, year: u16, day: u8) -> String {
        let stdout = process::Command::new("go")
            .args([
                "run",
                "main.go",
                "run",
                &format!("{}", year),
                &format!("{}", day),
            ])
            .current_dir(crate::base_path().join("golang"))
            .output()
            .unwrap()
            .stdout;

        String::from_utf8(stdout).expect("stdout should be valid UTF-8")
    }

    fn available_days(&self) -> HashMap<u16, Vec<u8>> {
        let output = process::Command::new("go")
            .args(["run", "main.go", "days"])
            .current_dir(crate::base_path().join("golang"))
            .stderr(Stdio::null())
            .output()
            .unwrap();

        parse_year_available_days(String::from_utf8_lossy(&output.stdout).as_ref())
    }

    fn prepare_day(&self, year: u16, day: u8) -> PathBuf {
        let year_package_file_path = crate::base_path().join(format!("golang/y{year}/y{year}.go"));
        if let Some(path) = year_package_file_path.parent() {
            fs::create_dir_all(path).unwrap();
        }

        if !year_package_file_path.exists() {
            let mut year_package_file = OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&year_package_file_path)
                .unwrap();

            year_package_file
                .write_all(
                    YEAR_TEMPLATE
                        .replace("$year", &format!("{}", year))
                        .as_bytes(),
                )
                .unwrap();

            let mut main_file = OpenOptions::new()
                .read(true)
                .write(true)
                .open(crate::base_path().join("golang/main.go"))
                .unwrap();

            crate::file::insert_codegen(
                &mut main_file,
                "import_year_package",
                &format!("\"com.github/augustoccesar/adventofcode/golang/y{year}\"\n"),
            );

            crate::file::insert_codegen(
                &mut main_file,
                "register_year",
                &format!("{year}: y{year}.DaysMap,\n"),
            );
        }

        let day_file_path = year_package_file_path
            .parent()
            .unwrap()
            .join(format!("d{day:0>2}/d{day:0>2}.go"));
        if let Some(path) = day_file_path.parent() {
            fs::create_dir_all(path).unwrap();
        }

        if !day_file_path.exists() {
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

            let mut year_package_file = OpenOptions::new()
                .read(true)
                .write(true)
                .open(year_package_file_path)
                .unwrap();

            crate::file::insert_codegen(
                &mut year_package_file,
                "import_day_package",
                &format!("\"com.github/augustoccesar/adventofcode/golang/y{year}/d{day:0>2}\"\n\t"),
            );

            crate::file::insert_codegen(
                &mut year_package_file,
                "register_day",
                &format!("1: &d{day:0>2}.Day{day:0>2}{{}},\n\t"),
            );
        }

        process::Command::new("go")
            .arg("fmt")
            .current_dir(crate::base_path().join("golang"))
            .status()
            .unwrap();

        day_file_path
    }

    fn path_to_day(&self, year: u16, day: u8) -> PathBuf {
        PathBuf::from_str(&format!("/golang/y{year}/d{day:0>2}/d{day:0>2}.go")).unwrap()
    }
}

const DAY_TEMPLATE: &str = r#"package d$padded_day

type Day$padded_day struct{}

func (d *Day$padded_day) Year() int { return $year }
func (d *Day$padded_day) Day() int  { return $day }

func (d *Day$padded_day) PartOne() string {
	return "-"
}

func (d *Day$padded_day) PartTwo() string {
	return "-"
}

"#;

const YEAR_TEMPLATE: &str = r#"package y$year

import (
	"com.github/augustoccesar/adventofcode/golang/structure"
	// CODEGEN:import_day_package
)

var DaysMap = map[int]structure.Day{
	// CODEGEN:register_day
}

"#;
