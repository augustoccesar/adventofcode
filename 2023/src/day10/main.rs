use std::{collections::HashSet, iter::FromIterator};

use aoc2023::{read_input, timed, Direction};

fn part_one() -> String {
    let map = parse_input(&read_input("10"));
    let path = traverse(&map);

    (path.len() / 2).to_string()
}

fn part_two() -> String {
    let map = parse_input(&read_input("10"));
    let path: HashSet<(usize, usize)> = HashSet::from_iter(traverse(&map));

    let mut inside_count = 0;
    for y in 0..map.len() {
        let mut is_inside = false;
        let mut last_char = '#';
        for x in 0..map[0].len() {
            match map[y][x] {
                current_char if path.contains(&(x, y)) => {
                    if current_char == '|'
                        || current_char == 'F'
                        || current_char == 'L'
                        || (current_char == 'J' && last_char == 'L')
                        || (current_char == '7' && last_char == 'F')
                    {
                        last_char = current_char;
                        is_inside ^= true;
                    }
                }
                _ if is_inside => inside_count += 1,
                _ => (),
            }
        }
    }

    inside_count.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn apply_pipe(direction: Direction, pipe: char) -> Option<Direction> {
    match pipe {
        '|' => Some(direction),
        '-' => Some(direction),
        'J' => match direction {
            Direction::East => Some(Direction::North),
            Direction::South => Some(Direction::West),
            _ => None,
        },
        'L' => match direction {
            Direction::West => Some(Direction::North),
            Direction::South => Some(Direction::East),
            _ => None,
        },
        '7' => match direction {
            Direction::East => Some(Direction::South),
            Direction::North => Some(Direction::West),
            _ => None,
        },
        'F' => match direction {
            Direction::West => Some(Direction::South),
            Direction::North => Some(Direction::East),
            _ => None,
        },
        _ => None,
    }
}

fn parse_input(input: &str) -> Vec<Vec<char>> {
    input
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>()
}

fn traverse(map: &[Vec<char>]) -> Vec<(usize, usize)> {
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

    let mut path: Vec<(usize, usize)> = Vec::new();
    path.push(start_point);

    let mut direction = Direction::East;
    let mut current_point = start_point;
    loop {
        let (modifier_x, modifier_y) = direction.modifier();
        current_point = (
            (current_point.0 as i32 + modifier_x) as usize,
            (current_point.1 as i32 + modifier_y) as usize,
        );

        if current_point == start_point {
            break;
        }

        let current_pipe = map[current_point.1][current_point.0];
        direction = apply_pipe(direction, current_pipe).unwrap();
        path.push(current_point);
    }

    path
}
