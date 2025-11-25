use md5::{Digest, Md5};

use crate::Day;

pub struct Day05 {}

impl Day for Day05 {
    fn year(&self) -> u16 {
        2016
    }

    fn day(&self) -> u8 {
        5
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();
        let door_id = input.as_str();
        let mut index = 0;
        let mut password: [char; 8] = ['-'; 8];

        for character in &mut password {
            let (next, hex) = find_next_interesting(door_id, index);

            *character = hex[5];
            index = next + 1;
        }

        String::from_iter(&password)
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();
        let door_id = input.as_str();
        let mut index = 0;
        let mut password: [char; 8] = ['-'; 8];

        while password.contains(&'-') {
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
                None => continue,
            }
        }

        String::from_iter(&password)
    }
}

fn generate_hash_hex(string: &str) -> String {
    let mut hasher = Md5::new();
    hasher.update(string);

    let result = hasher.finalize();
    hex::encode(&result[..])
}

fn find_next_interesting(id: &str, index: i32) -> (i32, Vec<char>) {
    let mut hex: Vec<char> = vec![];
    let mut new_index: i32 = index;

    while hex.is_empty() {
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

    (new_index, hex)
}
