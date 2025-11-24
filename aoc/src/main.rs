use std::path::PathBuf;

use clap::Parser;

mod file;

mod golang;
mod rust;

#[derive(Clone, Debug, clap::ValueEnum)]
enum Language {
    Rust,
    Golang,
}

impl ManagedLanguage for Language {
    fn prepare_day(&self, year: u16, day: u8) {
        match self {
            Language::Rust => rust::prepare_day(year, day),
            Language::Golang => golang::prepare_day(year, day),
        }
    }

    fn run(&self, year: u16, day: u8) {
        match self {
            Language::Rust => rust::run(year, day),
            Language::Golang => golang::run(year, day),
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
    year: u16,
    day: u8,
    #[clap(long)]
    language: Language,
}

impl PrepareDayArgs {
    fn handle(&self) {
        self.language.prepare_day(self.year, self.day);
    }
}

#[derive(clap::Args)]
struct RunArgs {
    year: u16,
    day: u8,
    #[clap(long)]
    language: Language,
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
