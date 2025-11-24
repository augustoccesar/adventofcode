use std::{fs::OpenOptions, io::Write, path::PathBuf};

use clap::Parser;

mod file;

mod cpp;
mod csharp;
mod golang;
mod java;
mod python;
mod ruby;
mod rust;
mod typescript;
mod zig;

#[derive(Clone, Debug, clap::ValueEnum)]
enum Language {
    Cpp,
    #[value(alias("cs"))]
    CSharp,
    #[value(alias("go"))]
    Golang,
    Java,
    #[value(alias("py"))]
    Python,
    #[value(alias("rb"))]
    Ruby,
    #[value(alias("rs"))]
    Rust,
    #[value(alias("ts"))]
    Typescript,
    Zig,
}

impl ManagedLanguage for Language {
    fn prepare_day(&self, year: u16, day: u8) {
        match self {
            Language::Cpp => cpp::prepare_day(year, day),
            Language::CSharp => csharp::prepare_day(year, day),
            Language::Golang => golang::prepare_day(year, day),
            Language::Java => java::prepare_day(year, day),
            Language::Python => python::prepare_day(year, day),
            Language::Ruby => ruby::prepare_day(year, day),
            Language::Rust => rust::prepare_day(year, day),
            Language::Typescript => typescript::prepare_day(year, day),
            Language::Zig => zig::prepare_day(year, day),
        }
    }

    fn run(&self, year: u16, day: u8) {
        match self {
            Language::Cpp => cpp::run(year, day),
            Language::CSharp => csharp::run(year, day),
            Language::Golang => golang::run(year, day),
            Language::Java => java::run(year, day),
            Language::Python => python::run(year, day),
            Language::Ruby => ruby::run(year, day),
            Language::Rust => rust::run(year, day),
            Language::Typescript => typescript::run(year, day),
            Language::Zig => zig::run(year, day),
        }
    }
}

trait ManagedLanguage {
    fn prepare_day(&self, year: u16, day: u8);
    fn run(&self, year: u16, day: u8);
}

#[derive(Parser)]
enum ManagementCli {
    PrepareDay(PrepareDayArgs),
    Run(RunArgs),
    DownloadInput(DownloadInputArgs),
}

#[derive(clap::Args)]
struct PrepareDayArgs {
    language: Language,
    year: u16,
    day: u8,
}

impl PrepareDayArgs {
    fn handle(&self) {
        self.language.prepare_day(self.year, self.day);
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
        self.language.run(self.year, self.day);
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
            .open(format!("../inputs/{}_{:0>2}.txt", self.year, self.day))
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
    }
}
