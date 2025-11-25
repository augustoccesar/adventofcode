use std::{collections::HashMap, str::FromStr};

use crate::Day;

pub struct Day02 {}

impl Day for Day02 {
    fn year(&self) -> u16 {
        2016
    }

    fn day(&self) -> u8 {
        2
    }

    fn part_one(&self) -> String {
        let input = parse_input(&self.read_default_input());
        let keypad = create_simple_keypad();
        let start_pos = (1, 1);

        find_pass(keypad, start_pos, input).join("")
    }

    fn part_two(&self) -> String {
        let input = parse_input(&self.read_default_input());
        let keypad = create_complex_keypad();
        let start_pos = (2, 0);

        find_pass(keypad, start_pos, input).join("")
    }
}

fn parse_input(input: &str) -> Vec<Vec<Direction>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| Direction::from_str(String::from(c).as_str()).unwrap())
                .collect()
        })
        .collect()
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

fn find_pass(
    keypad: HashMap<(i16, i16), String>,
    start_pos: (i16, i16),
    directions: Vec<Vec<Direction>>,
) -> Vec<String> {
    let mut pass: Vec<String> = vec![];
    let mut curr_pos: (i16, i16) = start_pos;

    for direction_set in directions {
        for direction in direction_set {
            let mut try_pos: (i16, i16) = curr_pos;

            match direction {
                Direction::U => try_pos.0 -= 1,
                Direction::R => try_pos.1 += 1,
                Direction::D => try_pos.0 += 1,
                Direction::L => try_pos.1 -= 1,
            }

            match keypad.get(&try_pos) {
                Some(_) => curr_pos = try_pos,
                None => continue,
            }
        }

        match keypad.get(&curr_pos) {
            Some(v) => pass.push(v.to_string()),
            None => unreachable!(),
        }
    }

    pass
}

fn create_simple_keypad() -> HashMap<(i16, i16), String> {
    // 1 2 3
    // 4 5 6
    // 7 8 9
    let mut keypad: HashMap<(i16, i16), String> = HashMap::new();
    keypad.insert((0, 0), String::from("1"));
    keypad.insert((0, 1), String::from("2"));
    keypad.insert((0, 2), String::from("3"));

    keypad.insert((1, 0), String::from("4"));
    keypad.insert((1, 1), String::from("5"));
    keypad.insert((1, 2), String::from("6"));

    keypad.insert((2, 0), String::from("7"));
    keypad.insert((2, 1), String::from("8"));
    keypad.insert((2, 2), String::from("9"));

    keypad
}

fn create_complex_keypad() -> HashMap<(i16, i16), String> {
    //     1
    //   2 3 4
    // 5 6 7 8 9
    //   A B C
    //     D
    // row, col
    let mut keypad: HashMap<(i16, i16), String> = HashMap::new();
    keypad.insert((0, 2), String::from("1"));

    keypad.insert((1, 1), String::from("2"));
    keypad.insert((1, 2), String::from("3"));
    keypad.insert((1, 3), String::from("4"));

    keypad.insert((2, 0), String::from("5"));
    keypad.insert((2, 1), String::from("6"));
    keypad.insert((2, 2), String::from("7"));
    keypad.insert((2, 3), String::from("8"));
    keypad.insert((2, 4), String::from("9"));

    keypad.insert((3, 1), String::from("A"));
    keypad.insert((3, 2), String::from("B"));
    keypad.insert((3, 3), String::from("C"));

    keypad.insert((4, 2), String::from("D"));

    keypad
}
