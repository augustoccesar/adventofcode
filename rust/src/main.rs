use std::{collections::HashMap, fs, process::exit};

use clap::Parser;

mod y2016;
mod y2023;
mod y2025;
// CODEGEN:year_module

pub trait Day {
    fn year(&self) -> u16;
    fn day(&self) -> u8;
    fn part_one(&self) -> String;
    fn part_two(&self) -> String;
    fn read_input(&self, name: &str) -> String {
        let path = if name.is_empty() {
            format!("../inputs/{}_{:0>2}.txt", self.year(), self.day(),)
        } else {
            format!("../inputs/{}_{:0>2}_{}.txt", self.year(), self.day(), name)
        };

        fs::read_to_string(path).unwrap()
    }
    fn read_default_input(&self) -> String {
        self.read_input("")
    }
}

#[derive(Parser)]
enum Cli {
    Run(RunArgs),
    Days,
}

#[derive(clap::Args)]
struct RunArgs {
    year: u16,
    day: u8,
}

fn main() {
    let days_map: HashMap<(u16, u8), Box<dyn Day>> = HashMap::from([
        ((2016, 1), Box::new(y2016::d01::Day01 {}) as Box<dyn Day>),
        ((2016, 10), Box::new(y2016::d10::Day10 {}) as Box<dyn Day>),
        ((2016, 2), Box::new(y2016::d02::Day02 {}) as Box<dyn Day>),
        ((2016, 3), Box::new(y2016::d03::Day03 {}) as Box<dyn Day>),
        ((2016, 4), Box::new(y2016::d04::Day04 {}) as Box<dyn Day>),
        ((2016, 5), Box::new(y2016::d05::Day05 {}) as Box<dyn Day>),
        ((2016, 6), Box::new(y2016::d06::Day06 {}) as Box<dyn Day>),
        ((2016, 7), Box::new(y2016::d07::Day07 {}) as Box<dyn Day>),
        ((2016, 8), Box::new(y2016::d08::Day08 {}) as Box<dyn Day>),
        ((2016, 9), Box::new(y2016::d09::Day09 {}) as Box<dyn Day>),
        ((2023, 1), Box::new(y2023::d01::Day01 {}) as Box<dyn Day>),
        ((2023, 10), Box::new(y2023::d10::Day10 {}) as Box<dyn Day>),
        ((2023, 11), Box::new(y2023::d11::Day11 {}) as Box<dyn Day>),
        ((2023, 12), Box::new(y2023::d12::Day12 {}) as Box<dyn Day>),
        ((2023, 13), Box::new(y2023::d13::Day13 {}) as Box<dyn Day>),
        ((2023, 14), Box::new(y2023::d14::Day14 {}) as Box<dyn Day>),
        ((2023, 15), Box::new(y2023::d15::Day15 {}) as Box<dyn Day>),
        ((2023, 16), Box::new(y2023::d16::Day16 {}) as Box<dyn Day>),
        ((2023, 17), Box::new(y2023::d17::Day17 {}) as Box<dyn Day>),
        ((2023, 18), Box::new(y2023::d18::Day18 {}) as Box<dyn Day>),
        ((2023, 19), Box::new(y2023::d19::Day19 {}) as Box<dyn Day>),
        ((2023, 2), Box::new(y2023::d02::Day02 {}) as Box<dyn Day>),
        ((2023, 3), Box::new(y2023::d03::Day03 {}) as Box<dyn Day>),
        ((2023, 4), Box::new(y2023::d04::Day04 {}) as Box<dyn Day>),
        ((2023, 5), Box::new(y2023::d05::Day05 {}) as Box<dyn Day>),
        ((2023, 6), Box::new(y2023::d06::Day06 {}) as Box<dyn Day>),
        ((2023, 7), Box::new(y2023::d07::Day07 {}) as Box<dyn Day>),
        ((2023, 8), Box::new(y2023::d08::Day08 {}) as Box<dyn Day>),
        ((2023, 9), Box::new(y2023::d09::Day09 {}) as Box<dyn Day>),
        ((2025, 1), Box::new(y2025::d01::Day01 {}) as Box<dyn Day>),
        ((2025, 2), Box::new(y2025::d02::Day02 {}) as Box<dyn Day>),
        ((2025, 3), Box::new(y2025::d03::Day03 {}) as Box<dyn Day>),
        ((2025, 4), Box::new(y2025::d04::Day04 {}) as Box<dyn Day>),
        ((2025, 5), Box::new(y2025::d05::Day05 {}) as Box<dyn Day>),
        ((2025, 6), Box::new(y2025::d06::Day06 {}) as Box<dyn Day>),
        // CODEGEN:day_map
    ]);

    let cli = Cli::parse();

    match cli {
        Cli::Run(run_args) => match days_map.get(&(run_args.year, run_args.day)) {
            Some(day_handler) => {
                let part_one_result = day_handler.part_one();
                println!("{part_one_result}");

                let part_two_result = day_handler.part_two();
                println!("{part_two_result}");
            }
            None => {
                eprintln!("Day {} not found for year {}", run_args.day, run_args.year);
                exit(1);
            }
        },
        Cli::Days => {
            let mut years: HashMap<u16, Vec<u8>> = HashMap::new();

            for (year, day) in days_map.keys() {
                years.entry(*year).or_default().push(*day);
            }

            for (year, mut days) in years {
                days.sort();

                let days_str = days
                    .iter()
                    .map(|day| day.to_string())
                    .collect::<Vec<_>>()
                    .join(";");

                println!("{};{}", year, days_str);
            }
        }
    }
}
