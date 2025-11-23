use std::path::PathBuf;

use clap::Parser;

mod file;
mod rust;

#[derive(Clone, Debug, clap::ValueEnum)]
enum Language {
    Rust,
}

impl ManagedLanguage for Language {
    fn prepare_day(&self, year: u16, day: u8) {
        match self {
            Language::Rust => rust::prepare_day(year, day),
        }
    }
}

trait ManagedLanguage {
    fn prepare_day(&self, year: u16, day: u8);
}

#[derive(Parser)]
enum ManagementCli {
    PrepareDay(PrepareDayArgs),
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
    }
}
