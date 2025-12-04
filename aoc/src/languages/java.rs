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
pub struct Java;

impl ManagedLanguage for Java {
    fn run(&self, year: u16, day: u8) {
        process::Command::new("gradle")
            .args(["run", &format!("--args=run {} {}", year, day)])
            .current_dir(crate::base_path().join("java"))
            .status()
            .unwrap();
    }

    fn available_days(&self) -> HashMap<u16, Vec<u8>> {
        let output = process::Command::new("gradle")
            .args(["run", "--args=days"])
            .current_dir(crate::base_path().join("java"))
            .stderr(Stdio::null())
            .output()
            .unwrap();

        parse_year_available_days(String::from_utf8_lossy(&output.stdout).as_ref())
    }

    fn prepare_day(&self, year: u16, day: u8) {
        let day_file_path = crate::base_path().join(format!(
            "java/app/src/main/java/se/augustocesar/aocjava/y{year}/Day{day:0>2}.java"
        ));

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
                        .replace("$day", &format!("{}", day))
                        .replace("$year", &format!("{}", year))
                        .as_bytes(),
                )
                .unwrap();

            // TODO: Think of a better way to do this. This is quite slow.
            // process::Command::new("gradle")
            //     .arg("spotlessApply")
            //     .current_dir(crate::base_path().join("java"))
            //     .status()
            //     .unwrap();
        }
    }

    fn path_to_day(&self, year: u16, day: u8) -> PathBuf {
        PathBuf::from_str(&format!(
            "/java/app/src/main/java/se/augustocesar/aocjava/y{year}/Day{day:0>2}.java"
        ))
        .unwrap()
    }
}

const DAY_TEMPLATE: &str = r#"package se.augustocesar.aocjava.y$year;

import se.augustocesar.aocjava.Day;
import se.augustocesar.aocjava.RunnableDay;

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
