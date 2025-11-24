use lazy_static::lazy_static;
use regex::Regex;

use crate::Day;

pub struct Day01 {}

impl Day for Day01 {
    fn year(&self) -> u16 {
        2023
    }

    fn day(&self) -> u8 {
        1
    }

    fn part_one(&self) -> String {
        self.read_default_input()
            .lines()
            .map(|line| line.chars().filter(|c| c.is_ascii_digit()).collect())
            .map(|digits: Vec<char>| {
                let digits = if digits.len() == 1 {
                    vec![*digits.first().unwrap(), *digits.first().unwrap()]
                } else if digits.len() > 2 {
                    vec![*digits.first().unwrap(), *digits.last().unwrap()]
                } else {
                    digits
                };

                digits.iter().collect::<String>()
            })
            .map(|digit| digit.parse::<i64>())
            .map(|result| result.unwrap())
            .sum::<i64>()
            .to_string()
    }

    fn part_two(&self) -> String {
        self.read_default_input()
            .lines()
            .map(|line| {
                let first = first_digit_char(line);
                let last = first_digit_char(&line.chars().rev().collect::<String>());

                [first, last].iter().collect::<String>()
            })
            .map(|digit| digit.parse::<i64>().unwrap())
            .sum::<i64>()
            .to_string()
    }
}

fn first_digit_char(str: &str) -> char {
    DIGIT_PATTERN
        .find(str)
        .map(|match_item| match_item.as_str())
        .map(|digit_str| {
            if digit_str.len() == 1 {
                digit_str.chars().next().unwrap()
            } else {
                digit_str_to_char(digit_str)
            }
        })
        .unwrap()
}

lazy_static! {
    static ref DIGIT_PATTERN: Regex =
        Regex::new(r"\d|one|eno|two|owt|three|eerht|four|ruof|five|evif|six|xis|seven|neves|eight|thgie|nine|enin").unwrap();
}

fn digit_str_to_char(digit_str: &str) -> char {
    match digit_str {
        "one" | "eno" => '1',
        "two" | "owt" => '2',
        "three" | "eerht" => '3',
        "four" | "ruof" => '4',
        "five" | "evif" => '5',
        "six" | "xis" => '6',
        "seven" | "neves" => '7',
        "eight" | "thgie" => '8',
        "nine" | "enin" => '9',
        _ => panic!("invalid digit literal"),
    }
}
