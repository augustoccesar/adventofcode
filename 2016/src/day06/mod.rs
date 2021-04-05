use crate::task::Task;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub struct Day06 {}

impl Task for Day06 {
    fn day(&self) -> String {
        return String::from("06");
    }

    fn part_one(&self) {
        let result = decode_message(self.read_input(), DECODE_MODE_MAX);

        println!("Part One: {}", result);
    }
    fn part_two(&self) {
        let result = decode_message(self.read_input(), DECODE_MODE_MIN);

        println!("Part Two: {}", result);
    }
}

// --------------------------------------------------------------------------------------------------------------------

type DecodeMode = i8;
const DECODE_MODE_MAX: DecodeMode = 1;
const DECODE_MODE_MIN: DecodeMode = 2;

fn decode_message(data: String, decode_mode: DecodeMode) -> String {
    let mut message_size: i8 = -1;
    let mut mapped_data: HashMap<usize, Vec<char>> = HashMap::new();

    data.lines().for_each(|line| {
        let chars: Vec<char> = line.chars().collect();
        if message_size == -1 {
            message_size = chars.len() as i8;
        }
        for i in 0..chars.len() {
            let entry: &mut Vec<char> = match mapped_data.entry(i) {
                Entry::Occupied(o) => o.into_mut(),
                Entry::Vacant(v) => v.insert(Vec::new()),
            };

            entry.push(chars[i]);
        }
    });

    let mut result: Vec<char> = Vec::new();
    for i in 0..message_size {
        let mut occurrences: HashMap<char, i8> = HashMap::new();
        let chars: &Vec<char> = mapped_data.get(&(i as usize)).unwrap();
        for item in chars {
            *occurrences.entry(*item).or_default() += 1;
        }

        let mut max: char = '-';
        if decode_mode == DECODE_MODE_MAX {
            max = occurrences
                .into_iter()
                .max_by_key(|(_, v)| *v)
                .map(|(k, _)| k)
                .unwrap();
        } else if decode_mode == DECODE_MODE_MIN {
            max = occurrences
                .into_iter()
                .min_by_key(|(_, v)| *v)
                .map(|(k, _)| k)
                .unwrap();
        }

        result.push(max);
    }

    return result.into_iter().collect::<String>();
}
