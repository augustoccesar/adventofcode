use std::{collections::HashSet, iter::zip, sync::mpsc::Sender, thread};

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

    let mut max_path = 0;

    let (tx, rx) = std::sync::mpsc::channel::<usize>();

    // Left starting
    calculate_max_direction(tx.clone(), contraption.clone(), Direction::East);

    // Top starting
    calculate_max_direction(tx.clone(), contraption.clone(), Direction::South);

    // Right starting
    calculate_max_direction(tx.clone(), contraption.clone(), Direction::West);

    // Bottom starting
    calculate_max_direction(tx.clone(), contraption.clone(), Direction::North);

    drop(tx);
    while let Ok(val) = rx.recv() {
        if val > max_path {
            max_path = val;
        }
    }

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

fn max_from_positions(
    contraption: &Vec<Vec<char>>,
    positions: &Vec<(i32, i32)>,
    direction: Direction,
) -> usize {
    let mut max_path = 0;

    positions.iter().for_each(|position| {
        let mut split_tracking: HashSet<(i32, i32)> = HashSet::new();

        let path = trace_path(
            contraption,
            (position.0, position.1),
            direction,
            &mut split_tracking,
        );

        if path.len() > max_path {
            max_path = path.len();
        }
    });

    max_path
}

fn calculate_max_direction(
    sender: Sender<usize>,
    contraption: Vec<Vec<char>>,
    direction: Direction,
) {
    thread::spawn(move || {
        let positions = starting_positions(&contraption, direction);
        let max_path = max_from_positions(&contraption, &positions, direction);

        sender.send(max_path).unwrap();
    });
}

fn starting_positions(contraption: &Vec<Vec<char>>, direction: Direction) -> Vec<(i32, i32)> {
    match direction {
        Direction::North => zip(
            0_i32..contraption[0].len() as i32,
            vec![contraption.len() as i32; contraption.len()],
        )
        .collect::<Vec<_>>(),
        Direction::East => zip(
            vec![-1_i32; contraption[0].len()],
            0_i32..contraption.len() as i32,
        )
        .collect::<Vec<_>>(),
        Direction::South => zip(
            0_i32..contraption[0].len() as i32,
            vec![-1; contraption.len()],
        )
        .collect::<Vec<_>>(),
        Direction::West => zip(
            vec![contraption[0].len() as i32; contraption[0].len()],
            0_i32..contraption.len() as i32,
        )
        .collect::<Vec<_>>(),
    }
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
