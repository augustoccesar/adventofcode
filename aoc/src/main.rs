use std::path::PathBuf;

use clap::Parser;

use crate::{
    commands::{
        CreateInputArgs, DownloadInputArgs, PrepareArgs, RunArgs, StatsArgs, UpdateReadmesArgs,
    },
    languages::Language,
};

mod commands;
mod file;
mod languages;

#[derive(Parser)]
enum ManagementCli {
    Prepare(PrepareArgs),
    Run(RunArgs),
    CreateInput(CreateInputArgs),
    DownloadInput(DownloadInputArgs),
    UpdateReadmes(UpdateReadmesArgs),
    Stats(StatsArgs),
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
        ManagementCli::Prepare(prepare_args) => prepare_args.handle(),
        ManagementCli::Run(run_args) => run_args.handle(),
        ManagementCli::CreateInput(create_input_args) => create_input_args.handle(),
        ManagementCli::DownloadInput(download_input_args) => download_input_args.handle(),
        ManagementCli::UpdateReadmes(update_readmes_args) => update_readmes_args.handle(),
        ManagementCli::Stats(stats_args) => stats_args.handle(),
    }
}
