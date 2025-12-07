use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::Day;

pub struct Day07 {}

impl Day for Day07 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        7
    }

    fn part_one(&self) -> String {
        let (manifold, start_position) = parse_input(&self.read_default_input());
        let mut split_cache = SplitCache::new();

        walk_beam(&manifold, &mut split_cache, start_position).to_string();

        split_cache.len().to_string()
    }

    fn part_two(&self) -> String {
        let (manifold, start_position) = parse_input(&self.read_default_input());
        let mut split_cache = SplitCache::new();

        walk_beam(&manifold, &mut split_cache, start_position).to_string()
    }
}

type Position = (usize, usize);
type Manifold = Vec<Vec<Tile>>;
type SplitCache = HashMap<Position, u64>;

fn parse_input(input: &str) -> (Manifold, Position) {
    let lines = input.lines().collect::<Vec<_>>();

    let height = lines.len();
    let width = lines[0].len();

    let mut start_position = (0, 0);
    let mut manifold = vec![vec![Tile::Empty; width]; height];
    for (y, row) in lines.iter().enumerate() {
        for (x, tile) in row.chars().map(Tile::from).enumerate() {
            if tile == Tile::Entry {
                start_position = (x, y);
            }

            manifold[y][x] = tile;
        }
    }

    (manifold, start_position)
}

fn walk_beam(manifold: &Manifold, split_cache: &mut SplitCache, mut pos: Position) -> u64 {
    loop {
        pos = (pos.0, pos.1 + 1);

        if let Some(timelines) = split_cache.get(&pos) {
            return *timelines;
        }

        if pos.1 == manifold.len() {
            return 1;
        }

        let next_tile = &manifold[pos.1][pos.0];

        match next_tile {
            Tile::Beam | Tile::Empty => (),
            Tile::Entry => unreachable!("Entry point should not be reachable again"),
            Tile::Splitter => {
                let timelines_left = walk_beam(manifold, split_cache, (pos.0 - 1, pos.1));
                let timelines_right = walk_beam(manifold, split_cache, (pos.0 + 1, pos.1));
                let total_timelines = timelines_left + timelines_right;

                split_cache.insert(pos, total_timelines);

                return timelines_left + timelines_right;
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Tile {
    Beam,
    Empty,
    Entry,
    Splitter,
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Beam => f.write_char('|'),
            Tile::Empty => f.write_char('.'),
            Tile::Entry => f.write_char('S'),
            Tile::Splitter => f.write_char('^'),
        }
    }
}

impl From<char> for Tile {
    fn from(value: char) -> Self {
        match value {
            '.' => Self::Empty,
            '|' => Self::Beam,
            '^' => Self::Splitter,
            'S' => Self::Entry,
            _ => panic!("invalid character"),
        }
    }
}
