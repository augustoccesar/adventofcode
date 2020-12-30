use std::fs;
use std::str::FromStr;

use crate::task::Task;

pub struct Day02 {}

impl Task for Day02 {
    fn part_one(&self) {
        let input = parse_input();
        let digits = find_digits(input);
        let res = digits.into_iter().map(|d: usize| d.to_string()).collect::<Vec<_>>().join("");

        println!("Part One: {}", res);
    }
    fn part_two(&self) {
        println!("Part Two");
    }
}

// --------------------------------------------------------------------------------------------------------------------

fn parse_input() -> Vec<Vec<Direction>> {
    let input = fs::read_to_string("inputs/day02_input.txt").unwrap();

    return input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| Direction::from_str(String::from(c).as_str()).unwrap())
                .collect()
        })
        .collect();
}

#[derive(Debug)]
enum Direction {
    U,
    R,
    D,
    L,
}

impl FromStr for Direction {
    type Err = ();

    fn from_str(input: &str) -> Result<Direction, Self::Err> {
        match input {
            "U" => Ok(Direction::U),
            "R" => Ok(Direction::R),
            "D" => Ok(Direction::D),
            "L" => Ok(Direction::L),
            _ => Err(()),
        }
    }
}

const NUM_PAD: [[usize; 3]; 3] = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

fn find_digits(directions: Vec<Vec<Direction>>) -> Vec<usize> {
    let mut digits = vec![];

    // row, col
    let mut curr_pos: (usize, usize) = (1, 1);

    for direction_set in directions {
        for direction in direction_set {
            match direction {
                Direction::U => match curr_pos.0.checked_sub(1) {
                    Some(new) => {
                        curr_pos.0 = new;
                    }
                    None => continue,
                },
                Direction::R => {
                    if curr_pos.1 + 1 < 3 {
                        curr_pos.1 += 1;
                    }
                }
                Direction::D => {
                    if curr_pos.0 + 1 < 3 {
                        curr_pos.0 += 1;
                    }
                }
                Direction::L => match curr_pos.1.checked_sub(1) {
                    Some(new) => {
                        curr_pos.1 = new;
                    }
                    None => continue
                    
                },
            }
        }

        let digit = NUM_PAD[curr_pos.0][curr_pos.1];
        digits.push(digit);
    }

    return digits;
}
