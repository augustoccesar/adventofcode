use std::{
    fs::{self, OpenOptions},
    io::Write,
    process,
};

pub fn run(year: u16, day: u8) {
    process::Command::new("uv")
        .args([
            "-q",
            "run",
            "main.py",
            "run",
            &format!("{}", year),
            &format!("{}", day),
        ])
        .current_dir(crate::base_path().join("python"))
        .status()
        .unwrap();
}

pub fn prepare_day(year: u16, day: u8) {
    let day_file_path = crate::base_path().join(&format!("python/y{year}/d{day:0>2}.py"));

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
}

const DAY_TEMPLATE: &str = r#"from day import Day, register_day

@register_day($year, $day)
class Day$padded_day(Day):
    def part_one(self) -> str:
        return "-"

    def part_two(self) -> str:
        return "-"
"#;
