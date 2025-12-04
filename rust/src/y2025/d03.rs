use std::{char, collections::HashMap};

use crate::Day;

pub struct Day03 {}

impl Day for Day03 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        3
    }

    fn part_one(&self) -> String {
        let banks = self
            .read_default_input()
            .lines()
            .map(|line| line.chars().map(char_to_digit).collect::<Vec<_>>())
            .collect::<Vec<Vec<u8>>>();

        let mut result: i64 = 0;
        for bank in banks {
            let mut largest = 0;

            for i in 0..bank.len() {
                for j in i + 1..bank.len() {
                    let value = bank[i] * 10 + bank[j];

                    if value > largest {
                        largest = value;
                    }
                }
            }

            result += largest as i64;
        }

        result.to_string()
    }

    fn part_two(&self) -> String {
        let banks = self
            .read_default_input()
            .lines()
            .map(|line| line.chars().map(char_to_digit).collect::<Vec<_>>())
            .collect::<Vec<Vec<u8>>>();

        let mut total = 0;
        for bank in &banks {
            let mut batteries_set = [0; 12];
            let mut from_idx = 0;

            for (i, battery) in batteries_set.iter_mut().enumerate() {
                let idx = next_largest_possible_starts(bank, from_idx, 12 - i);
                *battery = idx;

                from_idx = idx + 1;
            }

            total += batteries_set
                .map(|idx| digit_to_char(bank[idx]))
                .iter()
                .collect::<String>()
                .parse::<i64>()
                .unwrap();
        }

        total.to_string()
    }
}

fn char_to_digit(c: char) -> u8 {
    c.to_digit(10).unwrap() as u8
}

fn digit_to_char(digit: u8) -> char {
    char::from_digit(digit as u32, 10).unwrap()
}

fn next_largest_possible_starts(batteries: &[u8], from: usize, required_len: usize) -> usize {
    // From `from``, these are all the possible starting positions which can still fill the `required_len`
    let mut possible_starts: HashMap<u8, Vec<usize>> = HashMap::new();

    for position in from..batteries.len() {
        if batteries.len() - position < required_len {
            // From this position onwards, is not possible to get the necessary amount of batteries.
            break;
        }

        possible_starts
            .entry(batteries[position])
            .and_modify(|entry| entry.push(position))
            .or_insert(Vec::from([position]));
    }

    // From all the possible start positions, the one that will always result on the
    // largest number will be the first possibility of the largest digit, which
    // should always be only a single option.

    let largest_key = possible_starts.keys().max().unwrap();
    *possible_starts[largest_key].iter().min().unwrap()
}
