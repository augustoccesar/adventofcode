use std::{
    collections::{BTreeMap, HashMap},
    fs::OpenOptions,
    io::Write,
};

use clap::Parser;

use crate::{Language, base_path};

const CALENDAR_WIDTH_DAYS: usize = 5;

#[derive(Parser)]
pub struct Args {}

impl Args {
    pub fn handle(&self) {
        println!("Updating READMEs...");
        let data = build_readme_data();

        let mut readme = String::new();
        readme.push_str(README_HEADER);

        readme.push_str(&quick_links(&data));
        readme.push_str("\n\n");

        readme.push_str(&stats_section(&data));
        readme.push_str("\n");

        readme.push_str("## Calendars\n\n");

        for (year, days) in data.iter().rev() {
            readme.push_str(&year_calendar(*year, days));
        }

        readme.push_str(README_FOOTER);

        write_to_readme(&readme);
    }
}

type ReadmeData = BTreeMap<u16, Vec<Vec<Language>>>;

fn build_readme_data() -> ReadmeData {
    let mut data: ReadmeData = BTreeMap::new();
    for language in Language::all() {
        println!("\tFetching days done in {language}...");
        let available_days = language.managed().available_days();

        for (year, days) in &available_days {
            let days_count = match year {
                ..2025 => 25,
                2025.. => 12,
            };

            let data_days = data
                .entry(*year)
                .or_insert_with(|| vec![vec![]; days_count]);

            for day in days {
                data_days
                    .get_mut((*day - 1) as usize)
                    .unwrap()
                    .push(language.clone());
            }
        }
    }

    data
}

fn quick_links(data: &ReadmeData) -> String {
    data.keys()
        .rev()
        .map(|year| format!("[{year}](#user-content-{year}-expanded)"))
        .collect::<Vec<_>>()
        .join(" | ")
}

fn stats_section(data: &ReadmeData) -> String {
    let mut total_aoc_days = 0;
    let mut total_days_done = 0;

    let mut languages_days_count = HashMap::<&Language, usize>::new();
    for (_year, days) in data {
        for languages in days {
            total_aoc_days += 1;
            if languages.len() > 0 {
                total_days_done += 1;
            }

            for language in languages {
                languages_days_count
                    .entry(&language)
                    .and_modify(|entry| *entry += 1)
                    .or_insert(1);
            }
        }
    }

    let mut section = String::new();
    section.push_str("## Stats\n\n");

    section.push_str(&format!("**Total AoC days**: _{total_aoc_days}_</br>\n"));
    section.push_str(&format!("**Total days done**: _{total_days_done}_\n\n"));

    let mut sorted_languages: Vec<_> = languages_days_count.iter().collect();
    sorted_languages.sort_by(|a, b| b.1.cmp(a.1).then_with(|| a.0.cmp(b.0)));

    for (language, count) in sorted_languages {
        let percent = ((*count as f64 / total_days_done as f64) * 100.0).round() as u64;

        section.push_str(&format!(
            "- <img src=\"{}\" width=\"20\" height=\"20\"> **{}**: _{} days_ ({}%)\n",
            language.icon(),
            language,
            count,
            percent
        ));
    }

    section
}

fn year_calendar(year: u16, days: &[Vec<Language>]) -> String {
    let mut section = String::new();

    section.push_str("<details>\n\n");

    section.push_str(&format!("<summary><h3>{year}</h3></summary>\n\n"));
    section.push_str(&format!("<div id=\"{year}-expanded\">\n\n"));

    section.push('|');
    for _ in 0..CALENDAR_WIDTH_DAYS {
        section.push_str(" |");
    }
    section.push('\n');

    section.push('|');
    for _ in 0..CALENDAR_WIDTH_DAYS {
        section.push_str(" :---: |");
    }
    section.push('\n');

    for (chunk_idx, chunk) in days.chunks(CALENDAR_WIDTH_DAYS).enumerate() {
        section.push('|');
        for i in 0..chunk.len() {
            let day = (chunk_idx * CALENDAR_WIDTH_DAYS) + i + 1;
            section.push_str(&format!(" {:0>2} |", day));
        }
        section.push('\n');

        section.push('|');
        for (i, languages) in chunk.iter().enumerate() {
            let day = ((chunk_idx * CALENDAR_WIDTH_DAYS) + i + 1) as u8;

            if !languages.is_empty() {
                for language in languages {
                    section.push_str(&format!(
                        "<a href={:?}>",
                        language.managed().path_to_day(year, day)
                    ));
                    section.push_str(&format!(
                        "<img src=\"{}\" width=\"20\" height=\"20\">",
                        language.icon()
                    ));
                    section.push_str("</a>");
                }
            } else {
                section.push('—');
            }

            section.push_str("|");
        }

        section.push_str("\n");
    }

    section.push_str("</div>\n");
    section.push_str("</details>\n\n");

    section
}

fn write_to_readme(text: &str) {
    let mut readme_file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(base_path().join("README.md"))
        .unwrap();

    readme_file.write_all(text.as_bytes()).unwrap();
}

const README_HEADER: &str = "# 🎄🎅 Advent of Code 🎅🎄\n\n";
const README_FOOTER: &str = "## How does this repository works?

This repository is managed by the code under `/aoc`.
It is a program that helps generating and running days per language, and it has a
compiled binary (For ARM Mac only at the moment) under `/bin` so that it can be ran from it.

### Usual workflow
1. `./bin/aoc prepare-day <lang> <year> <day> --full`

   Prepare the day using `--full` so that it also download the input (if `AOC_SESSION` 
   environment variable is set).
   This will also update this README with links and update stats.

2. `./bin/aoc create-input <year> <day>`

   To create an empty example input so that can add the example that is usually
   given for the day.

3. `./bin/aoc run <lang> <year> <day>`

   This will run the day for the language with the correct toolchain.
";
