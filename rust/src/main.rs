use std::{collections::HashMap, env::args, fs, process::exit};

// CODEGEN:year_module

pub trait Day {
    fn day(&self) -> &'static str;
    fn part_one(&self) -> String;
    fn part_two(&self) -> String;
    fn read_input(&self, name: &str) -> String {
        return fs::read_to_string(format!("inputs/day{}_{}.txt", self.day(), name)).unwrap();
    }
    fn read_default_input(&self) -> String {
        self.read_input("input")
    }
}

fn main() {
    let days_map: HashMap<(u16, u8), Box<dyn Day>> = HashMap::from([
        // CODEGEN:day_map
    ]);

    // TODO: Better handling of args. Worth Clap?
    let args = args().collect::<Vec<_>>();
    if args.len() < 3 {
        eprintln!("Invalid amount of arguments");
        exit(1);
    }

    let year = &args[0]
        .parse::<u16>()
        .expect("Year argument is not a valid u16");
    let day = &args[1]
        .parse::<u8>()
        .expect("Day argument is not a valid u8");

    match days_map.get(&(*year, *day)) {
        Some(day_handler) => {
            let part_one_result = day_handler.part_one();
            println!("Part one: {part_one_result}");

            let part_two_result = day_handler.part_two();
            println!("Part two: {part_two_result}");
        }
        None => {
            eprintln!("Day {day} not found for year {year}");
            exit(1);
        }
    }
}
