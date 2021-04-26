use crate::task::Task;

use lazy_static::lazy_static;
use regex::Regex;

pub struct Day08 {}

impl Task for Day08 {
    fn day(&self) -> String {
        return String::from("08");
    }

    fn part_one(&self) {
        let commands = parse_input(self.read_input());
        let mut display: Display = Display::init_with_dimension(50, 6);

        for command in commands {
            if command.cmd == "rect" {
                display.rect(command.parameter_1, command.parameter_2);
            } else if command.cmd == "rotate column" {
                display.rotate_col(command.parameter_1, command.parameter_2);
            } else if command.cmd == "rotate row" {
                display.rotate_row(command.parameter_1, command.parameter_2);
            } else {
                panic!("Invalid command: {}", command.cmd);
            }
        }

        // display.show();

        println!("Part One: {}", &display.lit_leds().to_string());
    }

    fn part_two(&self) {
        println!("Part Two: {}", "-");
    }
}

// --------------------------------------------------------------------------------------------------------------------

lazy_static! {
    static ref RE_RECT_COMMAND: Regex = Regex::new(r"(rect)\s(\d+)x(\d+)").unwrap();
    static ref RE_ROTATE_COMMAND: Regex =
        Regex::new(r"(rotate\s(?:row|column))\s(?:x|y)=(\d+)\sby\s(\d+)").unwrap();
}

#[derive(Debug)]
struct Command {
    cmd: String,
    parameter_1: usize,
    parameter_2: usize,
}

impl Command {
    fn from_str(string: &str) -> Command {
        let mut cmd: String = String::new();
        let mut parameter_1 = 0;
        let mut parameter_2 = 0;

        if RE_RECT_COMMAND.is_match(string) {
            for cap in RE_RECT_COMMAND.captures_iter(string) {
                cmd = String::from(&cap[1]);
                parameter_1 = cap[2].to_string().parse().unwrap();
                parameter_2 = cap[3].to_string().parse().unwrap();
            }
        } else if RE_ROTATE_COMMAND.is_match(string) {
            for cap in RE_ROTATE_COMMAND.captures_iter(string) {
                cmd = String::from(&cap[1]);
                parameter_1 = cap[2].to_string().parse().unwrap();
                parameter_2 = cap[3].to_string().parse().unwrap();
            }
        } else {
            panic!("received invalid command: {}", string);
        }

        return Command {
            cmd,
            parameter_1,
            parameter_2,
        };
    }
}

struct Display {
    leds: Vec<Vec<bool>>,
}

impl Display {
    fn init_with_dimension(x: usize, y: usize) -> Display {
        return Display {
            leds: vec![vec![false; x]; y],
        };
    }

    fn show(self) {
        for row in self.leds {
            for col in row {
                let sign = if col { "# " } else { ". " };
                print!("{}", sign);
            }

            println!();
        }
    }

    fn rect(&mut self, x: usize, y: usize) {
        for i in 0..y {
            for j in 0..x {
                self.leds[i][j] = true;
            }
        }
    }

    fn rotate_row(&mut self, row: usize, amount: usize) {
        let row_size = self.leds[row].len();

        let mut original_leds = vec![false; row_size];

        for (i, col) in self.leds[row].iter().enumerate() {
            original_leds[i] = *col;
        }

        let shifted_leds = shift_vector(original_leds, amount);

        for (i, col) in self.leds[row].iter_mut().enumerate() {
            *col = shifted_leds[i];
        }
    }

    fn rotate_col(&mut self, col: usize, amount: usize) {
        let mut original_leds = vec![false; self.leds.len()];
        for (i, row) in self.leds.iter().enumerate() {
            original_leds[i] = row[col];
        }

        let shifted_leds = shift_vector(original_leds, amount);

        for (i, row) in self.leds.iter_mut().enumerate() {
            row[col] = shifted_leds[i];
        }
    }

    fn lit_leds(self) -> usize {
        let flattened_leds = self.leds.iter().flatten().cloned().collect::<Vec<bool>>();
        let lit_leds = flattened_leds
            .into_iter()
            .filter(|led| *led)
            .collect::<Vec<_>>()
            .len();
        return lit_leds;
    }
}

fn shift_vector<T: Default + Copy>(vector: Vec<T>, shift: usize) -> Vec<T> {
    let mut shifted = vec![T::default(); vector.len()];

    for (i, item) in vector.iter().enumerate() {
        let new_idx = (i + shift) % vector.len();
        shifted[new_idx] = *item;
    }

    return shifted;
}

fn parse_input(string: String) -> Vec<Command> {
    let mut commands: Vec<Command> = vec![];
    for line in string.lines() {
        commands.push(Command::from_str(line));
    }

    return commands;
}
