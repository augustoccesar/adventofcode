use std::{
    fs::{self, OpenOptions},
    io::Write,
    process::{self, Stdio},
};

pub fn run(year: u16, day: u8) {
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

pub fn prepare_day(year: u16, day: u8) {
    let day_file_path = crate::base_path().join(&format!("csharp/y{year}/Day{day:0>2}.cs"));

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

        process::Command::new("dotnet")
            .arg("format")
            .current_dir(crate::base_path().join("csharp"))
            .status()
            .unwrap();
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
