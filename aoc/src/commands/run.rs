use std::time::Duration;

use anyhow::{Context, anyhow};

use crate::languages::Language;

#[derive(clap::Args)]
pub struct Args {
    language: Language,
    year: u16,
    day: u8,
}

impl Args {
    pub fn handle(&self) {
        let output = self.language.managed().run(self.year, self.day);

        match parse_output(&output) {
            Ok(results) => {
                for (i, (result, duration)) in results.enumerate() {
                    let name = match i {
                        0 => "one",
                        1 => "two",
                        _ => unreachable!("there should always be at most two parts"),
                    };

                    println!("Part {name}: {result} ({duration:?})");
                }
            }
            Err(error) => {
                eprintln!("Failed to parse output: {error}");
                eprintln!("Raw output:\n");
                eprintln!("{output}");
            }
        }
    }
}

fn parse_output(output: &str) -> Result<impl Iterator<Item = (&str, Duration)>, anyhow::Error> {
    let lines = output.lines().collect::<Vec<_>>();

    if lines.len() != 2 {
        return Err(anyhow!("Output should have exactly two lines"));
    }

    let results: Result<Vec<_>, anyhow::Error> = lines
        .into_iter()
        .map(|line| {
            let mut line_iter = line.split(";");
            let result = line_iter.next().expect("line should have a result");
            let duration = Duration::from_nanos(
                line_iter
                    .next()
                    .context("line should have a duration")?
                    .parse::<u64>()
                    .context("duration should be a valid u64")?,
            );

            Ok((result, duration))
        })
        .collect();

    Ok(results?.into_iter())
}
