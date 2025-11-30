use std::{collections::BTreeMap, fs::OpenOptions, io::Write};

use clap::Parser;

use crate::{Language, base_path};

const CALENDAR_WIDTH_DAYS: usize = 5;

#[derive(Parser)]
pub struct Args {}

impl Args {
    pub fn handle(&self) {
        let mut data: BTreeMap<u16, [Vec<Language>; 25]> = BTreeMap::new();
        for language in Language::all() {
            let available_days = language.managed().available_days();

            for (year, days) in &available_days {
                let data_days = data
                    .entry(*year)
                    .or_insert_with(|| std::array::from_fn(|_| Vec::new()));

                for day in days {
                    data_days
                        .get_mut((*day - 1) as usize)
                        .unwrap()
                        .push(language.clone());
                }
            }
        }

        let mut readme = String::new();
        readme.push_str(README_HEADER);

        for (year, days) in data {
            readme.push_str(&format!("## {year}\n\n"));
            readme.push('|');
            for _ in 0..CALENDAR_WIDTH_DAYS {
                readme.push_str(" |");
            }
            readme.push('\n');

            readme.push('|');
            for _ in 0..CALENDAR_WIDTH_DAYS {
                readme.push_str(" :---: |");
            }
            readme.push('\n');

            for (chunk_idx, chunk) in days.chunks(CALENDAR_WIDTH_DAYS).enumerate() {
                readme.push('|');
                for i in 0..chunk.len() {
                    let day = (chunk_idx * CALENDAR_WIDTH_DAYS) + i + 1;
                    readme.push_str(&format!(" {:0>2} |", day));
                }
                readme.push('\n');

                readme.push('|');
                for (i, languages) in chunk.iter().enumerate() {
                    let day = ((chunk_idx * CALENDAR_WIDTH_DAYS) + i + 1) as u8;

                    if !languages.is_empty() {
                        for language in languages {
                            readme.push_str(&format!(
                                "<a href={:?}>",
                                language.managed().path_to_day(year, day)
                            ));
                            readme.push_str(&format!(
                                "<img src=\"resources/icons/{}.svg\" width=\"20\" height=\"20\">",
                                language
                            ));
                            readme.push_str("</a>");
                        }
                    } else {
                        readme.push('—');
                    }

                    readme.push_str("|");
                }

                readme.push_str("\n");
            }

            readme.push_str("\n");
        }

        readme.push_str(README_FOOTER);

        let mut readme_file = OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(base_path().join("README.md"))
            .unwrap();

        readme_file.write_all(readme.as_bytes()).unwrap();
    }
}

const README_HEADER: &str = "# 🎄🎅 Advent of Code 🎅🎄\n\n";
const README_FOOTER: &str = "## How does this repository works?

This repository is managed by the code under `/aoc`.
It is a program that helps generating and running days per language, and it has a
compiled binary (For ARM Mac only at the moment) under `/bin` so that it can be ran from it.
";
