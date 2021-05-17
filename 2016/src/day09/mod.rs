use crate::task::Task;

use lazy_static::lazy_static;
use regex::Regex;
use std::iter::FromIterator;

pub struct Day09 {}

impl Task for Day09 {
    fn day(&self) -> String {
        return String::from("09");
    }

    fn part_one(&self) {
        let input = self.read_input();
        let output = decompress(input, false);

        println!("Part One: {}", output);
    }

    fn part_two(&self) {
        let input = self.read_input();
        let output = decompress(input, true);

        println!("Part Two: {}", output);
    }
}

// --------------------------------------------------------------------------------------------------------------------

lazy_static! {
    static ref RE_RANGE: Regex = Regex::new(r"\((\d+)x(\d+)\)").unwrap();
}

fn decompress(string: String, nested: bool) -> usize {
    let chars: Vec<_> = string.chars().collect();
    let mut size = 0;

    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '(' {
            let mut end: usize = 0;
            let mut repeat_size: usize = 0;
            let mut repeat_count: usize = 0;

            for j in i..chars.len() {
                if chars[j] == ')' {
                    end = j;
                    break;
                }
            }

            let range_str = String::from_iter(&chars[i..=end]);
            for cap in RE_RANGE.captures_iter(&range_str) {
                repeat_size = cap[1].parse::<usize>().unwrap();
                repeat_count = cap[2].parse::<usize>().unwrap();
            }

            let repeat_str = String::from_iter(&chars[end + 1..=end + repeat_size]);
            if repeat_str.contains("(") && nested {
                let deep_size = decompress(repeat_str, nested);
                size += deep_size * repeat_count;
            } else {
                size += repeat_str.len() * repeat_count;
            }

            i = end + 1 + repeat_size;
        } else {
            size += 1;
            i += 1;
        }
    }

    return size;
}
