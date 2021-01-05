use crate::task::Task;
use md5::{Md5, Digest};
use std::iter::FromIterator;

pub struct Day05 {}

impl Task for Day05 {
    fn day(&self) -> String {
        return String::from("05");
    }

    fn part_one(&self) {
        let input = self.read_input();
        let door_id = input.as_str();
        let mut index = 0;
        let mut password: [char; 8] = ['-'; 8];

        for i in 0..8 {
            let (next, hex) = find_next_interesting(door_id, index);

            password[i] = hex[5];
            index = next + 1;
        }

        println!("Part One: {}", String::from_iter(&password));
    }
    fn part_two(&self) {
        let input = self.read_input();
        let door_id = input.as_str();
        let mut index = 0;
        let mut password: [char; 8] = ['-'; 8];

        while password.iter().any(|x| *x == '-') {
            let (next, hex) = find_next_interesting(door_id, index);

            index = next + 1;
            match hex[5].to_digit(10) {
                Some(val) => {
                    let pass_idx = val as usize;
                    if pass_idx > 7 || password[pass_idx] != '-' {
                        continue;
                    }

                    password[pass_idx] = hex[6];
                }
                None => continue
            }
        }


        println!("Part Two: {}", String::from_iter(&password));
    }
}

// --------------------------------------------------------------------------------------------------------------------

fn generate_hash_hex(string: &str) -> String {
    let mut hasher = Md5::new();
    hasher.update(string);

    let result = hasher.finalize();
    let wot = &result[..];
    return hex::encode(wot);
}

fn find_next_interesting(id: &str, index: i32) -> (i32, Vec<char>) {
    let mut hex: Vec<char> = vec![];
    let mut new_index: i32 = index;

    while hex.len() == 0 {
        let mut id_string: String = id.to_string();
        id_string.push_str(new_index.to_string().as_str());
        let hash_hex_chars = generate_hash_hex(id_string.as_str())
            .chars()
            .collect::<Vec<char>>();

        if hash_hex_chars[0..5].iter().all(|x| *x == '0') {
            hex = hash_hex_chars;
        }

        new_index += 1;
    }

    return (new_index, hex);
}
