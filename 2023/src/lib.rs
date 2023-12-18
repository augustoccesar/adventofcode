use std::{convert::TryFrom, fs, time::Instant};

pub fn timed<F>(function: F)
where
    F: FnOnce() -> String,
{
    let start = Instant::now();
    let result = function();
    let duration = start.elapsed();
    println!("{}", result);
    println!("({:?})\n", duration);
}

pub fn read_input(day: &str) -> String {
    fs::read_to_string(format!("inputs/day{}_input.txt", day)).unwrap()
}

pub fn read_named_input(day: &str, name: &str) -> String {
    fs::read_to_string(format!("inputs/day{}_{}.txt", day, name)).unwrap()
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    pub fn iter() -> impl Iterator<Item = Direction> {
        vec![
            Direction::North,
            Direction::West,
            Direction::South,
            Direction::East,
        ]
        .into_iter()
    }

    pub const fn modifier(&self) -> (i32, i32) {
        match self {
            Direction::North => (0, -1),
            Direction::East => (1, 0),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
        }
    }

    pub const fn opposite(&self) -> Self {
        match self {
            Direction::North => Direction::South,
            Direction::East => Direction::West,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
        }
    }
}

impl TryFrom<char> for Direction {
    type Error = String;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'U' | 'N' => Ok(Self::North),
            'R' | 'E' => Ok(Self::East),
            'D' | 'S' => Ok(Self::South),
            'L' | 'W' => Ok(Self::West),
            _ => Err(String::from("invalid direction char")),
        }
    }
}
