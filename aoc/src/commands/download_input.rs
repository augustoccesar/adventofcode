use std::{fs::OpenOptions, io::Write};

use crate::base_path;

#[derive(clap::Args)]
pub struct Args {
    pub year: u16,
    pub day: u8,
    pub aoc_session: String,
}

impl Args {
    pub fn handle(&self) {
        let input = ureq::get(format!(
            "https://adventofcode.com/{}/day/{}/input",
            self.year, self.day
        ))
        .header("user-agent", "github.com/augustoccesar/adventofcode")
        .header("cookie", format!("session={}", self.aoc_session))
        .call()
        .unwrap()
        .body_mut()
        .read_to_string()
        .unwrap();

        let input = input.trim_ascii();

        let mut input_file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(base_path().join(format!("inputs/{}_{:0>2}.txt", self.year, self.day)))
            .unwrap();

        input_file.write_all(input.as_bytes()).unwrap();
    }
}
