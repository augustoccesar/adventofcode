use crate::task::Task;
use md5::{Md5, Digest};
use std::iter::FromIterator;

pub struct Day05 {}

impl Task for Day05 {
    fn day(&self) -> std::string::String {
        return String::from("05");
    }

    fn part_one(&self) {
        let input = self.read_input();
        let door_id = input.as_str();
        let mut index = 0;
        let mut password: Vec<char> = vec![];

        while password.len() < 8 {
            let (next, pass_char) = find_next_interesting(door_id, index);

            password.push(pass_char);
            index = next + 1;
        }

        println!("Part One: {:?}", String::from_iter(password));
    }
    fn part_two(&self) {
        println!("Part Two");
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

fn find_next_interesting(id: &str, index: i32) -> (i32, char) {
    let mut pass_char: char = '.';
    let mut new_index: i32 = index;

    while pass_char == '.' {
        let mut id_string: String = id.to_string();
        id_string.push_str(new_index.to_string().as_str());
        let hash_hex_chars = generate_hash_hex(id_string.as_str())
            .chars()
            .collect::<Vec<char>>();

        if hash_hex_chars[0..5].iter().all(|x| *x == '0') {
            pass_char = hash_hex_chars[5];
        }

        new_index += 1;
    }

    return (new_index, pass_char);
}
