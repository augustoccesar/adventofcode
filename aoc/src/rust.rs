use std::{
    fs::{self, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
    process,
};

pub fn run(year: u16, day: u8) {
    process::Command::new("cargo")
        .args([
            "run",
            "--",
            "run",
            &format!("{}", year),
            &format!("{}", day),
        ])
        .current_dir(crate::base_path().join("rust"))
        .status()
        .unwrap();
}

pub fn prepare_day(year: u16, day: u8) {
    let year_module_path = crate::base_path().join(&format!("rust/src/y{year}/mod.rs"));

    if !year_module_path.exists() {
        create_year_module(year, &year_module_path);
    }

    let mut day_module_path = PathBuf::from(year_module_path.parent().unwrap());
    day_module_path.push(format!("d{day:0>2}.rs"));

    if !day_module_path.exists() {
        create_day_module(year, day, &year_module_path, &day_module_path);
    }

    process::Command::new("cargo")
        .arg("fmt")
        .current_dir(crate::base_path().join("rust"))
        .status()
        .unwrap();
}

fn create_year_module(year: u16, year_module_path: &Path) {
    // Create the base module for the year
    year_module_path
        .parent()
        .map(|path| fs::create_dir_all(path).unwrap());

    let mut year_module = OpenOptions::new()
        .write(true)
        .create(true)
        .open(year_module_path)
        .unwrap();

    year_module
        .write_all("// CODEGEN:day_module\n".as_bytes())
        .unwrap();

    // Update the main file with the new year module
    let mut main_file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(crate::base_path().join("rust/src/main.rs"))
        .unwrap();

    crate::file::insert_codegen(&mut main_file, "year_module", &format!("mod y{year};\n"));
}

fn create_day_module(year: u16, day: u8, year_module_path: &Path, day_module_path: &Path) {
    // Create the base module for the day
    day_module_path
        .parent()
        .map(|path| fs::create_dir_all(path).unwrap());

    // Write the template for a day
    let mut day_module = OpenOptions::new()
        .write(true)
        .create(true)
        .open(day_module_path)
        .unwrap();

    day_module
        .write_all(
            DAY_TEMPLATE
                .replace("$padded_day", &format!("{day:0>2}"))
                .replace("$day", &format!("{day}"))
                .replace("$year", &format!("{year}"))
                .as_bytes(),
        )
        .unwrap();

    // Update the year module to include the new day
    let mut year_module = OpenOptions::new()
        .read(true)
        .write(true)
        .open(year_module_path)
        .unwrap();

    crate::file::insert_codegen(
        &mut year_module,
        "day_module",
        &format!("pub mod d{day:0>2};\n"),
    );

    // Update the main file with the new day
    let mut main_file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(crate::base_path().join("rust/src/main.rs"))
        .unwrap();

    crate::file::insert_codegen(
        &mut main_file,
        "day_map",
        &format!(
            "(({year}, {day}), Box::new(y{year}::d{day:0>2}::Day{day:0>2} {{}}) as Box<dyn Day>),\n"
        ),
    );
}

const DAY_TEMPLATE: &str = r#"use crate::Day;

pub struct Day$padded_day {}

impl Day for Day$padded_day {
    fn year(&self) -> u16 {
        $year
    }

    fn day(&self) -> u8 {
        $day
    }

    fn part_one(&self) -> String {
        "-".to_owned()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}
"#;
