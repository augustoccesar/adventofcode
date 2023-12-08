use std::collections::HashMap;

use aoc2023::{read_input, read_named_input, timed};
use lazy_static::lazy_static;
use regex::Regex;

fn part_one() -> String {
    let lines = read_input("08")
        .lines()
        .map(|line| line.to_string())
        .collect::<Vec<String>>();

    let instructions = lines[0].chars().collect::<Vec<char>>();
    let mut map: HashMap<String, (String, String)> = HashMap::new();
    for line in lines[2..].iter() {
        let captures = NODES_REGEX.captures(line).unwrap();
        let node = captures.get(1).unwrap().as_str().to_string();
        let left = captures.get(2).unwrap().as_str().to_string();
        let right = captures.get(3).unwrap().as_str().to_string();
        map.insert(node, (left, right));
    }

    let mut steps = 0;
    let mut i = 0;
    let mut current_node = &String::from("AAA");
    loop {
        let instruction = instructions[i];
        let (left, right) = map.get(current_node).unwrap();
        if instruction == 'L' {
            current_node = left;
        } else {
            current_node = right;
        }

        steps += 1;

        if current_node == "ZZZ" {
            break;
        }

        if i == instructions.len() - 1 {
            i = 0
        } else {
            i += 1;
        }
    }

    steps.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}

lazy_static! {
    static ref NODES_REGEX: Regex =
        Regex::new(r"([A-Z]{3})\s=\s\(([A-Z]{3}),\s([A-Z]{3})\)").expect("failed to compile regex");
}
