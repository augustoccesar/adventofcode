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
        let map = self
            .read_default_input()
            .lines()
            .map(|line| line.chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>();

        let mut result = 0;
        for (y, row) in map.iter().enumerate() {
            for (x, cell) in row.iter().enumerate() {
                if *cell != '@' {
                    continue;
                }

                let neighbor_rolls = neighbors(&map, x as i32, y as i32)
                    .iter()
                    .filter(|neighbor_cell| **neighbor_cell == '@')
                    .count();

                if neighbor_rolls < 4 {
                    result += 1;
                }
            }
        }

        result.to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}

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

fn neighbors(map: &[Vec<char>], x: i32, y: i32) -> [char; 8] {
    let mut neighbors = ['-'; 8];
    let position = (x, y);

    for (i, direction) in Direction::all().enumerate() {
        let modifier = direction.modifier();
        let neighbor_pos = (position.0 + modifier.0, position.1 + modifier.1);

        if neighbor_pos.0 < 0 || neighbor_pos.1 < 0 {
            continue;
        }

        if neighbor_pos.0 >= map[0].len() as i32 || neighbor_pos.1 >= map.len() as i32 {
            continue;
        }

        neighbors[i] = map[neighbor_pos.1 as usize][neighbor_pos.0 as usize]
    }

    neighbors
}
