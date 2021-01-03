use crate::task::Task;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;
use std::iter::FromIterator;

lazy_static! {
    static ref ROOM_RE: Regex = Regex::new(r"([a-z-]+)-(\d+)\[(\w+)\]").unwrap();
}

pub struct Day04 {}

impl Task for Day04 {
    fn day(&self) -> std::string::String {
        return String::from("04");
    }

    fn part_one(&self) {
        let input = self.read_input();
        let mut sector_id_sum: i32 = 0;

        for cap in ROOM_RE.captures_iter(&input) {
            let name = &cap[1];
            let sector_id = &cap[2].parse::<i16>().unwrap();
            let checksum = &cap[3];

            let expected_checksum = generate_checksum(name);

            if checksum == expected_checksum {
                sector_id_sum += i32::from(*sector_id);
            }
        }

        println!("Part One: {:?}", sector_id_sum);
    }
    fn part_two(&self) {
        println!("Part Two");
    }
}

// --------------------------------------------------------------------------------------------------------------------

fn generate_checksum(string: &str) -> String {
    let mut count: HashMap<char, i16> = HashMap::new();

    for c in string.chars() {
        if !c.is_alphabetic() {
            continue;
        }

        match count.get_mut(&c) {
            Some(curr_val) => *curr_val += 1,
            None => {
                count.insert(c, 1);
            }
        }
    }

    let mut char_pairs: Vec<_> = count.into_iter().collect();
    // Sort the values descending then sort the keys ascending
    char_pairs.sort_by(|a, b| a.1.cmp(&b.1).reverse().then(a.0.cmp(&b.0)));
    let checksum = char_pairs[0..=4].iter().map(|x| x.0).collect::<Vec<_>>();

    return String::from_iter(checksum);
}
