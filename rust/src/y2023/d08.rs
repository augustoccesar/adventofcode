use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

use crate::Day;

pub struct Day08 {}

impl Day for Day08 {
    fn year(&self) -> u16 {
        2023
    }

    fn day(&self) -> u8 {
        8
    }

    fn part_one(&self) -> String {
        let (map, instructions) = parse_input(&self.read_default_input());

        let mut steps = 0;
        let mut instruction_idx = 0;
        let mut current_node = "AAA";
        loop {
            if current_node == "ZZZ" {
                break;
            }

            let instruction = instructions[instruction_idx];
            let (left, right) = map
                .get(current_node)
                .expect("all nodes should exist on the map");

            match instruction {
                'L' => current_node = left,
                'R' => current_node = right,
                _ => unreachable!(),
            }

            steps += 1;
            instruction_idx = (instruction_idx + 1) % instructions.len();
        }

        steps.to_string()
    }

    fn part_two(&self) -> String {
        let (map, instructions) = parse_input(&self.read_default_input());
        let current_nodes = map
            .keys()
            .filter(|node| node.ends_with('A'))
            .collect::<Vec<&String>>();

        let mut cycles = vec![0_u64; current_nodes.len()];
        for idx in 0..current_nodes.len() {
            let mut node = current_nodes[idx];

            let mut step = 0;
            let mut instruction_idx = 0;
            loop {
                let instruction = instructions[instruction_idx];

                if node.ends_with('Z') && cycles[idx] == 0 {
                    cycles[idx] = step;
                    break;
                }

                let (left, right) = map.get(node).expect("all nodes should exist on the map");
                match instruction {
                    'L' => node = left,
                    'R' => node = right,
                    _ => unreachable!(),
                }

                step += 1;
                instruction_idx = (instruction_idx + 1) % instructions.len();
            }
        }

        lcm(&cycles).to_string()
    }
}

lazy_static! {
    static ref NODES_REGEX: Regex =
        Regex::new(r"([A-Z]{3})\s=\s\(([A-Z]{3}),\s([A-Z]{3})\)").expect("failed to compile regex");
}

type Map = HashMap<String, (String, String)>;

fn parse_input(input: &str) -> (Map, Vec<char>) {
    let lines = input
        .lines()
        .map(|line| line.to_string())
        .collect::<Vec<String>>();

    let instructions = lines[0].chars().collect::<Vec<char>>();
    let mut map: Map = Map::new();
    for line in lines[2..].iter() {
        let captures = NODES_REGEX.captures(line).unwrap();
        let node = captures.get(1).unwrap().as_str().to_string();
        let left = captures.get(2).unwrap().as_str().to_string();
        let right = captures.get(3).unwrap().as_str().to_string();
        map.insert(node, (left, right));
    }

    (map, instructions)
}

fn lcm(values: &[u64]) -> u64 {
    values
        .iter()
        .fold(1, |acc, &item| acc * item / gcd(acc, item))
}

fn gcd(mut left: u64, mut right: u64) -> u64 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    left
}
