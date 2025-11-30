use std::{fs::OpenOptions, io::Write, path::PathBuf};

use clap::Parser;

use crate::{commands::UpdateReadmesArgs, languages::Language};

mod commands;
mod file;
mod languages;

#[derive(Parser)]
enum ManagementCli {
    PrepareDay(PrepareDayArgs),
    Run(RunArgs),
    DownloadInput(DownloadInputArgs),
    UpdateReadmes(UpdateReadmesArgs),
}

#[derive(clap::Args)]
struct PrepareDayArgs {
    language: Language,
    year: u16,
    day: u8,
}

impl PrepareDayArgs {
    fn handle(&self) {
        self.language.managed().prepare_day(self.year, self.day);
    }
}

#[derive(clap::Args)]
struct RunArgs {
    language: Language,
    year: u16,
    day: u8,
}

impl RunArgs {
    fn handle(&self) {
        self.language.managed().run(self.year, self.day);
    }
}

#[derive(clap::Args)]
struct DownloadInputArgs {
    year: u16,
    day: u8,
    aoc_session: String,
}

impl DownloadInputArgs {
    fn handle(&self) {
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

pub fn base_path() -> PathBuf {
    if std::env::var("CARGO").is_ok() {
        PathBuf::from("../")
    } else {
        PathBuf::from("./")
    }
}

fn main() {
    let cli = ManagementCli::parse();
    match cli {
        ManagementCli::PrepareDay(prepare_day_args) => prepare_day_args.handle(),
        ManagementCli::Run(run_args) => run_args.handle(),
        ManagementCli::DownloadInput(download_input_args) => download_input_args.handle(),
        ManagementCli::UpdateReadmes(update_readmes_args) => update_readmes_args.handle(),
    }
}
