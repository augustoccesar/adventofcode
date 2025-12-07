use std::env;

use crate::{commands, languages::Language};

#[derive(clap::Args)]
pub struct Args {
    language: Language,
    year: u16,
    day: u8,
    /// If should do the whole preparation.
    ///
    /// If false, it will only create the necessary files.
    /// If true, will create the files, download the input and update the README file.
    #[clap(long)]
    full: bool,
}

impl Args {
    pub fn handle(&self) {
        println!(
            "Preparing {} files for {}-{:0>2}",
            self.language, self.year, self.day
        );

        let day_file_path = self.language.managed().prepare_day(self.year, self.day);

        if self.full {
            commands::UpdateReadmesArgs {}.handle();

            if let Ok(aoc_session) = env::var("AOC_SESSION") {
                println!("Found AOC_SESSION environment variable! Downloading input.");
                commands::DownloadInputArgs {
                    year: self.year,
                    day: self.day,
                    aoc_session,
                }
                .handle();
            }
        }

        println!(
            "Day ready at: {}",
            day_file_path
                .to_str()
                .expect("Day file path should be valid str")
        )
    }
}
