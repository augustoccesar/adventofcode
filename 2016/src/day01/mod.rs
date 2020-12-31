use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs;

use crate::task::Task;

pub struct Day01 {}

impl Task for Day01 {
    fn day(&self) -> std::string::String {
        return String::from("01");
    }

    fn part_one(&self) {
        let commands: Vec<Command> = parse_input();
        let mut facing: usize = 0;
        let mut point: Point2D = Point2D::new(0, 0);

        for command in commands {
            update_facing(&mut facing, command.direction);
            let direction = DIRECTIONS[facing];

            point.x = point.x + (command.amount * direction.0);
            point.y = point.y + (command.amount * direction.1);
        }

        println!("Part One: {:?}", point.taxicab_distance(Point2D::new(0, 0)));
    }
    fn part_two(&self) {
        let commands: Vec<Command> = parse_input();
        let mut facing: usize = 0;
        let mut point: Point2D = Point2D::new(0, 0);
        let mut pos_history: HashMap<String, bool> = HashMap::new();

        'outer: for command in commands {
            update_facing(&mut facing, command.direction);
            let direction = DIRECTIONS[facing];

            for _ in 0..command.amount {
                point.x = point.x + direction.0;
                point.y = point.y + direction.1;

                match pos_history.get(point.hash().as_str()) {
                    Some(_) => {
                        break 'outer;
                    }
                    None => {
                        pos_history.insert(String::from(point.hash()), true);
                        continue;
                    }
                }
            }
        }

        println!("Part Two: {:?}", point.taxicab_distance(Point2D::new(0, 0)));
    }
}

// --------------------------------------------------------------------------------------------------------------------

const DIRECTIONS: [(i16, i16); 4] = [
    // x, y
    (0, 1),  // N - 0
    (1, 0),  // E - 1
    (0, -1), // S - 2
    (-1, 0), // W - 3
];

fn wrap_index(desired: i16, size: i16) -> usize {
    let u_index = u16::try_from(((desired + size) % size) % size).unwrap();

    return usize::from(u_index);
}

fn update_facing(was: &mut usize, turn: String) {
    if turn == "R" {
        *was = wrap_index(i16::try_from(*was).unwrap() + 1, 4);
    } else if turn == "L" {
        *was = wrap_index(i16::try_from(*was).unwrap() - 1, 4);
    }
}

fn parse_input() -> Vec<Command> {
    let input = fs::read_to_string("inputs/day01_input.txt").unwrap();
    return input
        .split(", ")
        .map(|cmd| Command::parse(cmd))
        .collect::<Vec<Command>>();
}

#[derive(Debug)]
struct Command {
    direction: String,
    amount: i16,
}

impl Command {
    pub fn parse(input: &str) -> Command {
        let direction = &input[0..1];
        let amount = input[1..].parse::<i16>().unwrap();

        return Command {
            direction: String::from(direction),
            amount: amount,
        };
    }
}

#[derive(Debug)]
struct Point2D {
    x: i16,
    y: i16,
}

impl Point2D {
    pub fn new(x: i16, y: i16) -> Point2D {
        return Point2D { x: x, y: y };
    }
    fn hash(&self) -> String {
        return format!("{},{}", self.x, self.y);
    }
    fn taxicab_distance(&self, other: Point2D) -> i16 {
        return (self.x - other.x).abs() + (self.y - other.y).abs();
    }
}
