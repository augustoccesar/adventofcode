use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    let map = read_input("10")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let start_point = map
        .iter()
        .enumerate()
        .find_map(|(y, row)| {
            row.iter().enumerate().find_map(
                |(x, item)| {
                    if *item == 'S' {
                        Some((x, y))
                    } else {
                        None
                    }
                },
            )
        })
        .unwrap();

    let mut direction = Direction::East;
    let mut current_point = start_point;
    let mut steps = 0;
    loop {
        steps += 1;

        let (modifier_x, modifier_y) = direction.modifier();
        current_point = (
            (current_point.0 as i32 + modifier_x) as usize,
            (current_point.1 as i32 + modifier_y) as usize,
        );

        if current_point == start_point {
            break;
        }

        let current_pipe = map[current_point.1][current_point.0];
        direction = direction.apply_pipe(current_pipe).unwrap();
    }

    (steps / 2).to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}

#[derive(PartialEq, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn modifier(&self) -> (i32, i32) {
        match self {
            Direction::North => (0, -1),
            Direction::East => (1, 0),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
        }
    }

    fn apply_pipe(self, pipe: char) -> Option<Self> {
        match pipe {
            '|' => Some(self),
            '-' => Some(self),
            'J' => match self {
                Direction::East => Some(Direction::North),
                Direction::South => Some(Direction::West),
                _ => None,
            },
            'L' => match self {
                Direction::West => Some(Direction::North),
                Direction::South => Some(Direction::East),
                _ => None,
            },
            '7' => match self {
                Direction::East => Some(Direction::South),
                Direction::North => Some(Direction::West),
                _ => None,
            },
            'F' => match self {
                Direction::West => Some(Direction::South),
                Direction::North => Some(Direction::East),
                _ => None,
            },
            _ => None,
        }
    }
}
