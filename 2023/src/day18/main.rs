use aoc2023::{read_input, timed};

fn part_one() -> String {
    calculate_area(&parse_input(&read_input("18"), 1)).to_string()
}

fn part_two() -> String {
    calculate_area(&parse_input(&read_input("18"), 2)).to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn parse_input(input: &str, part: u8) -> Vec<(Direction, i64)> {
    input
        .lines()
        .map(|line| match part {
            1 => {
                let tokens = line.split(' ').collect::<Vec<_>>();
                let direction = Direction::from_char(tokens[0].chars().next().unwrap());
                let steps = tokens[1].parse::<i64>().unwrap();

                (direction, steps)
            }
            2 => {
                let tokens = line.split('#').collect::<Vec<_>>();
                let hex = &tokens[1][0..tokens[1].len() - 1];
                let steps = i64::from_str_radix(&hex[0..hex.len() - 1], 16).unwrap();
                let direction =
                    Direction::from((hex[hex.len() - 1..hex.len()]).parse::<u8>().unwrap());

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

// TODO(augustoccesar)[2021-10-03]: Move this to a common module
#[derive(PartialEq, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    const fn modifier(&self) -> (i32, i32) {
        match self {
            Direction::North => (0, -1),
            Direction::East => (1, 0),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
        }
    }

    const fn from_char(c: char) -> Self {
        match c {
            'U' => Self::North,
            'R' => Self::East,
            'D' => Self::South,
            'L' => Self::West,
            _ => panic!("invalid direction char"),
        }
    }
}

impl From<u8> for Direction {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::East,
            1 => Self::South,
            2 => Self::West,
            3 => Self::North,
            _ => panic!("invalid direction u8"),
        }
    }
}
