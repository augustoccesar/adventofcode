use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    read_input("15")
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(hash)
        .sum::<u32>()
        .to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn hash(input: &str) -> u32 {
    let chars = input.chars().map(|c| c as u32).collect::<Vec<u32>>();

    let mut current_value = 0;
    for c in chars {
        current_value += c;
        current_value *= 17;
        current_value %= 256;
    }

    current_value
}
