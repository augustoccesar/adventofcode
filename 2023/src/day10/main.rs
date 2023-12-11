use std::{collections::HashSet, iter::FromIterator};

use aoc2023::{read_input, timed};

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
        direction = direction.apply_pipe(current_pipe).unwrap();
        path.push(current_point);
    }

    path
}
