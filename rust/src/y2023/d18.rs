use crate::{Day, y2023::Direction};

pub struct Day18 {}

impl Day for Day18 {
    fn year(&self) -> u16 {
        2023
    }

    fn day(&self) -> u8 {
        18
    }

    fn part_one(&self) -> String {
        calculate_area(&parse_input(&self.read_default_input(), 1)).to_string()
    }

    fn part_two(&self) -> String {
        calculate_area(&parse_input(&self.read_default_input(), 2)).to_string()
    }
}

fn parse_input(input: &str, part: u8) -> Vec<(Direction, i64)> {
    input
        .lines()
        .map(|line| match part {
            1 => {
                let tokens = line.split(' ').collect::<Vec<_>>();
                let direction = Direction::try_from(tokens[0].chars().next().unwrap()).unwrap();
                let steps = tokens[1].parse::<i64>().unwrap();

                (direction, steps)
            }
            2 => {
                let tokens = line.split('#').collect::<Vec<_>>();
                let hex = &tokens[1][0..tokens[1].len() - 1];
                let steps = i64::from_str_radix(&hex[0..hex.len() - 1], 16).unwrap();
                let direction =
                    direction_from_u8((hex[hex.len() - 1..hex.len()]).parse::<u8>().unwrap());
                (direction, steps)
            }
            _ => panic!("invalid part"),
        })
        .collect::<Vec<_>>()
}

fn calculate_area(instructions: &[(Direction, i64)]) -> i64 {
    // https://artofproblemsolving.com/wiki/index.php/Shoelace_Theorem
    let mut area = 0;
    let mut perimeter = 0;
    let mut current_position = (0, 0);
    for instruction in instructions {
        let modifier = instruction.0.modifier();
        let next_position = (
            current_position.0 + (modifier.0 as i64 * instruction.1),
            current_position.1 + (modifier.1 as i64 * instruction.1),
        );

        area += (current_position.1 + next_position.1) * (next_position.0 - current_position.0);
        perimeter += instruction.1;

        current_position = next_position;
    }

    (area / 2).abs() + (perimeter / 2) + 1
}

fn direction_from_u8(value: u8) -> Direction {
    match value {
        0 => Direction::East,
        1 => Direction::South,
        2 => Direction::West,
        3 => Direction::North,
        _ => panic!("invalid direction u8"),
    }
}
