use std::{
    collections::HashSet,
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
        let (mut manifold, start_position) = parse_input(&self.read_default_input());
        let mut splits_hit = 0;

        let mut active_beams: HashSet<Position> = HashSet::new();
        active_beams.insert((start_position.0, start_position.1 + 1));

        loop {
            let mut new_active_beams = HashSet::<Position>::new();

            for beam in active_beams.iter() {
                let new_position = (beam.0, beam.1 + 1);
                let tile_in_new_position = &manifold[new_position.1][new_position.0];

                match tile_in_new_position {
                    Tile::Beam => (), // Noop?
                    Tile::Empty => {
                        new_active_beams.insert(new_position);
                        manifold[new_position.1][new_position.0] = Tile::Beam;
                    }
                    Tile::Splitter => {
                        splits_hit += 1;
                        new_active_beams.insert((new_position.0 + 1, new_position.1));
                        manifold[new_position.1][new_position.0 + 1] = Tile::Beam;
                        new_active_beams.insert((new_position.0 - 1, new_position.1));
                        manifold[new_position.1][new_position.0 - 1] = Tile::Beam;
                    }
                    Tile::Entry => {
                        unreachable!("moving beam should not be able to hit the entry again")
                    }
                }
            }

            active_beams = new_active_beams;

            if active_beams.iter().map(|beam| beam.1).next().unwrap() == manifold.len() - 1 {
                break;
            }
        }

        splits_hit.to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}

type Position = (usize, usize);
type Manifold = Vec<Vec<Tile>>;

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

#[allow(dead_code)]
fn print_manifold(manifold: &Manifold) {
    for row in manifold.iter() {
        for tile in row.iter() {
            print!("{}", tile);
        }
        println!()
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
