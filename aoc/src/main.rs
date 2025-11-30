use std::path::PathBuf;

use clap::Parser;

use crate::{
    commands::{DownloadInputArgs, PrepareDayArgs, RunArgs, UpdateReadmesArgs},
    languages::Language,
};

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
