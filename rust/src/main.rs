use std::{collections::HashMap, fs, process::exit};

use clap::Parser;

mod y2016;
mod y2023;
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
}

#[derive(clap::Args)]
struct RunArgs {
    year: u16,
    day: u8,
}

fn main() {
    let days_map: HashMap<(u16, u8), Box<dyn Day>> = HashMap::from([
        ((2023, 1), Box::new(y2023::d01::Day01 {}) as Box<dyn Day>),
        ((2016, 1), Box::new(y2016::d01::Day01 {}) as Box<dyn Day>),
        ((2016, 2), Box::new(y2016::d02::Day02 {}) as Box<dyn Day>),
        ((2016, 3), Box::new(y2016::d03::Day03 {}) as Box<dyn Day>),
        ((2016, 4), Box::new(y2016::d04::Day04 {}) as Box<dyn Day>),
        ((2016, 5), Box::new(y2016::d05::Day05 {}) as Box<dyn Day>),
        ((2016, 6), Box::new(y2016::d06::Day06 {}) as Box<dyn Day>),
        ((2016, 7), Box::new(y2016::d07::Day07 {}) as Box<dyn Day>),
        ((2016, 8), Box::new(y2016::d08::Day08 {}) as Box<dyn Day>),
        ((2016, 9), Box::new(y2016::d09::Day09 {}) as Box<dyn Day>),
        ((2016, 10), Box::new(y2016::d10::Day10 {}) as Box<dyn Day>),
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
    }
}
