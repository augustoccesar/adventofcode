use std::{
    fs::{self, OpenOptions},
    io::Write,
    process::{self, Stdio},
};

pub fn run(year: u16, day: u8) {
    process::Command::new("gradle")
        .args(["run", &format!("--args=run {} {}", year, day)])
        .current_dir(crate::base_path().join("java"))
        .stderr(Stdio::null())
        .status()
        .unwrap();
}

pub fn prepare_day(year: u16, day: u8) {
    let day_file_path = crate::base_path().join(&format!(
        "java/app/src/main/java/com/augustoccesar/aocjava/y{year}/Day{day:0>2}.java"
    ));

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

        process::Command::new("gradle")
            .arg("spotlessApply")
            .current_dir(crate::base_path().join("java"))
            .status()
            .unwrap();
    }
}

const DAY_TEMPLATE: &str = r#"package com.augustoccesar.aocjava.y$year;

import com.augustoccesar.aocjava.Day;
import com.augustoccesar.aocjava.RunnableDay;

@RunnableDay(year = $year, day = $day)
public class Day$padded_day extends Day {
    @Override
    public String partOne() {
        return "-";
    }

    @Override
    public String partTwo() {
        return "-";
    }
}

"#;
