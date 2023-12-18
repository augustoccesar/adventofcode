use aoc2023::{read_input, timed};

fn part_one() -> String {
    let instructions = read_input("18")
        .lines()
        .map(|line| {
            let tokens = line.split(' ').collect::<Vec<_>>();
            let direction = Direction::from_char(tokens[0].chars().next().unwrap());
            let steps = tokens[1].parse::<i32>().unwrap();

            (direction, steps)
        })
        .collect::<Vec<_>>();

    // https://artofproblemsolving.com/wiki/index.php/Shoelace_Theorem
    let mut area = 0;
    let mut perimeter = 0;
    let mut current_position = (0, 0);
    for instruction in instructions {
        let modifier = instruction.0.modifier();
        let next_position = (
            current_position.0 + (modifier.0 * instruction.1),
            current_position.1 + (modifier.1 * instruction.1),
        );

        area += (current_position.1 + next_position.1) * (next_position.0 - current_position.0);
        perimeter += instruction.1;

        current_position = next_position;
    }

    ((area / 2).abs() + (perimeter / 2) + 1).to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
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
