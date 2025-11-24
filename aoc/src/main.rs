use std::path::PathBuf;

use clap::Parser;

mod file;

mod cpp;
mod golang;
mod java;
mod python;
mod rust;
mod typescript;

#[derive(Clone, Debug, clap::ValueEnum)]
enum Language {
    Cpp,
    #[value(alias("go"))]
    Golang,
    Java,
    #[value(alias("py"))]
    Python,
    #[value(alias("rs"))]
    Rust,
    #[value(alias("ts"))]
    Typescript,
}

impl ManagedLanguage for Language {
    fn prepare_day(&self, year: u16, day: u8) {
        match self {
            Language::Cpp => cpp::prepare_day(year, day),
            Language::Golang => golang::prepare_day(year, day),
            Language::Java => java::prepare_day(year, day),
            Language::Python => python::prepare_day(year, day),
            Language::Rust => rust::prepare_day(year, day),
            Language::Typescript => typescript::prepare_day(year, day),
        }
    }

    fn run(&self, year: u16, day: u8) {
        match self {
            Language::Cpp => cpp::run(year, day),
            Language::Golang => golang::run(year, day),
            Language::Java => java::run(year, day),
            Language::Python => python::run(year, day),
            Language::Rust => rust::run(year, day),
            Language::Typescript => typescript::run(year, day),
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
    }
}
