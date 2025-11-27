pub mod d01;
pub mod d02;
pub mod d03;
pub mod d04;
pub mod d05;
pub mod d06;
pub mod d07;
pub mod d08;
pub mod d09;
pub mod d10;
pub mod d11;
pub mod d12;
pub mod d13;
pub mod d14;
pub mod d15;
pub mod d16;
pub mod d17;
pub mod d18;
pub mod d19;
// CODEGEN:day_module

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
