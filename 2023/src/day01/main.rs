use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    read_input("01")
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

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}
