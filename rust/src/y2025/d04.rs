use crate::Day;

pub struct Day04 {}

impl Day for Day04 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        4
    }

    fn part_one(&self) -> String {
        let map = parse_input(&self.read_default_input());

        let mut result = 0;
        for (x, y) in rolls_positions(&map) {
            if count_neighbor_rolls(&map, x, y) < 4 {
                result += 1;
            }
        }

        result.to_string()
    }

    fn part_two(&self) -> String {
        let mut map = parse_input(&self.read_default_input());

        let mut removed = 0;
        loop {
            let mut to_be_removed = Vec::<Position>::new();

            for (x, y) in rolls_positions(&map) {
                if count_neighbor_rolls(&map, x, y) < 4 {
                    to_be_removed.push((x, y));
                }
            }

            if to_be_removed.is_empty() {
                break;
            }

            for remove_pos in &to_be_removed {
                map[remove_pos.1][remove_pos.0] = '.';
                removed += 1;
            }
        }

        removed.to_string()
    }
}

type Map = Vec<Vec<char>>;
type Position = (usize, usize);

enum Direction {
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW,
}

impl Direction {
    fn modifier(&self) -> (i32, i32) {
        match self {
            Direction::N => (0, -1),
            Direction::NE => (1, -1),
            Direction::E => (1, 0),
            Direction::SE => (1, 1),
            Direction::S => (0, 1),
            Direction::SW => (-1, 1),
            Direction::W => (-1, 0),
            Direction::NW => (-1, -1),
        }
    }

    fn all() -> impl Iterator<Item = Direction> {
        [
            Direction::N,
            Direction::NE,
            Direction::E,
            Direction::SE,
            Direction::S,
            Direction::SW,
            Direction::W,
            Direction::NW,
        ]
        .into_iter()
    }
}

fn parse_input(input: &str) -> Map {
    input
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>()
}

fn rolls_positions(map: &Map) -> Vec<Position> {
    let mut positions = Vec::new();

    for (y, row) in map.iter().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            if *cell == '@' {
                positions.push((x, y));
            }
        }
    }

    positions
}

fn count_neighbor_rolls(map: &[Vec<char>], x: usize, y: usize) -> usize {
    let mut neighbor_rolls = 0;
    let position = (x, y);

    for direction in Direction::all() {
        let modifier = direction.modifier();
        let neighbor_pos = (
            position.0 as i32 + modifier.0,
            position.1 as i32 + modifier.1,
        );

        if neighbor_pos.0 < 0 || neighbor_pos.1 < 0 {
            continue;
        }

        if neighbor_pos.0 >= map[0].len() as i32 || neighbor_pos.1 >= map.len() as i32 {
            continue;
        }

        let neighbor_cell = map[neighbor_pos.1 as usize][neighbor_pos.0 as usize];
        if neighbor_cell == '@' {
            neighbor_rolls += 1;
        }
    }

    neighbor_rolls
}
