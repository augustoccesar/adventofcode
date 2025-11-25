use lazy_static::lazy_static;
use regex::Regex;

use crate::Day;

pub struct Day09 {}

impl Day for Day09 {
    fn year(&self) -> u16 {
        2016
    }

    fn day(&self) -> u8 {
        9
    }

    fn part_one(&self) -> String {
        decompress(self.read_default_input(), false).to_string()
    }

    fn part_two(&self) -> String {
        decompress(self.read_default_input(), true).to_string()
    }
}

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

            for (j, item) in chars.iter().enumerate().skip(i) {
                if *item == ')' {
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

    size
}
