use std::{collections::HashSet, iter::zip};

use aoc2023::{read_input, timed};

fn part_one() -> String {
    let contraption = read_input("16")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let mut split_tracking: HashSet<(i32, i32)> = HashSet::new();

    let path = trace_path(&contraption, (-1, 0), Direction::East, &mut split_tracking);

    path.len().to_string()
}

fn part_two() -> String {
    let contraption = read_input("16")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let all_y = (0..contraption.len())
        .map(|item| item as i32)
        .collect::<Vec<_>>();
    let all_x = (0..contraption[0].len())
        .map(|item| item as i32)
        .collect::<Vec<_>>();

    let mut max_path = 0;

    // Left starting
    zip(vec![-1; all_x.len()], &all_y)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|position| {
            let mut split_tracking: HashSet<(i32, i32)> = HashSet::new();
            let path = trace_path(
                &contraption,
                (position.0, *position.1),
                Direction::East,
                &mut split_tracking,
            );

            if path.len() > max_path {
                max_path = path.len();
            }
        });

    // Top starting
    zip(&all_x, vec![-1; all_y.len()])
        .collect::<Vec<_>>()
        .iter()
        .for_each(|position| {
            let mut split_tracking: HashSet<(i32, i32)> = HashSet::new();
            let path = trace_path(
                &contraption,
                (*position.0, position.1),
                Direction::South,
                &mut split_tracking,
            );

            if path.len() > max_path {
                max_path = path.len();
            }
        });

    // Right starting
    zip(vec![contraption[0].len() as i32; all_x.len()], &all_y)
        .collect::<Vec<_>>()
        .iter()
        .for_each(|position| {
            let mut split_tracking: HashSet<(i32, i32)> = HashSet::new();
            let path = trace_path(
                &contraption,
                (position.0, *position.1),
                Direction::West,
                &mut split_tracking,
            );

            if path.len() > max_path {
                max_path = path.len();
            }
        });

    // Bottom starting
    zip(&all_x, vec![contraption.len() as i32; all_y.len()])
        .collect::<Vec<_>>()
        .iter()
        .for_each(|position| {
            let mut split_tracking: HashSet<(i32, i32)> = HashSet::new();
            let path = trace_path(
                &contraption,
                (*position.0, position.1),
                Direction::North,
                &mut split_tracking,
            );

            if path.len() > max_path {
                max_path = path.len();
            }
        });

    max_path.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn trace_path(
    contraption: &Vec<Vec<char>>,
    starting_pos: (i32, i32),
    direction: Direction,
    split_tracking: &mut HashSet<(i32, i32)>,
) -> HashSet<(i32, i32)> {
    let mut path: HashSet<(i32, i32)> = HashSet::new();

    let mut direction = direction;
    let mut current_pos = starting_pos;
    loop {
        let (next_x, next_y) = (
            current_pos.0 + direction.modifier().0,
            current_pos.1 + direction.modifier().1,
        );

        if next_y >= contraption.len() as i32 || next_y < 0 {
            break;
        }

        if next_x >= contraption[0].len() as i32 || next_x < 0 {
            break;
        }

        path.insert((next_x, next_y));

        current_pos = (next_x, next_y);
        let value = contraption[current_pos.1 as usize][current_pos.0 as usize];
        match (value, direction) {
            ('.', _) => (),
            ('-', Direction::North | Direction::South) => {
                match split_tracking.contains(&current_pos) {
                    true => break,
                    false => split_tracking.insert(current_pos),
                };

                let west_path =
                    trace_path(contraption, current_pos, Direction::West, split_tracking);
                let east_path =
                    trace_path(contraption, current_pos, Direction::East, split_tracking);

                path.extend(west_path);
                path.extend(east_path);
                break;
            }
            ('|', Direction::East | Direction::West) => {
                match split_tracking.contains(&current_pos) {
                    true => break,
                    false => split_tracking.insert(current_pos),
                };

                let north_path =
                    trace_path(contraption, current_pos, Direction::North, split_tracking);
                let south_path =
                    trace_path(contraption, current_pos, Direction::South, split_tracking);

                path.extend(north_path);
                path.extend(south_path);
                break;
            }
            ('-' | '|', _) => (),
            ('/', Direction::North) => direction = Direction::East,
            ('/', Direction::East) => direction = Direction::North,
            ('/', Direction::South) => direction = Direction::West,
            ('/', Direction::West) => direction = Direction::South,
            ('\\', Direction::North) => direction = Direction::West,
            ('\\', Direction::East) => direction = Direction::South,
            ('\\', Direction::South) => direction = Direction::East,
            ('\\', Direction::West) => direction = Direction::North,
            _ => unreachable!(),
        }
    }

    path
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
}
