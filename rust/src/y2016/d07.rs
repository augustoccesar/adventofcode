use std::collections::HashMap;

use crate::Day;

pub struct Day07 {}

impl Day for Day07 {
    fn year(&self) -> u16 {
        2016
    }

    fn day(&self) -> u8 {
        7
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();

        let mut count = 0;
        for ip in input.lines() {
            if support_tls(ip) {
                count += 1;
            }
        }

        count.to_string()
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();

        let mut count = 0;
        for ip in input.lines() {
            if support_ssl(ip) {
                count += 1;
            }
        }

        count.to_string()
    }
}

fn support_tls(ip: &str) -> bool {
    let mut contains_inside_brackets = false;
    let mut contains_outside_brackets = false;
    let mut current_inside_brackets = false;

    let chars: Vec<_> = ip.chars().collect();
    for (i, c) in chars.iter().enumerate() {
        if *c == '[' {
            current_inside_brackets = true;
            continue;
        } else if *c == ']' {
            current_inside_brackets = false;
            continue;
        }

        if i + 3 >= ip.len() {
            break;
        }

        if (chars[i + 3] == *c && chars[i + 1] == chars[i + 2]) && chars[i + 1] != *c {
            match current_inside_brackets {
                true => contains_inside_brackets = true,
                false => contains_outside_brackets = true,
            }
        }
    }

    contains_outside_brackets && !contains_inside_brackets
}

fn support_ssl(ip: &str) -> bool {
    let mut aba_registry: HashMap<String, bool> = HashMap::new();
    let mut bab_registry: HashMap<String, bool> = HashMap::new();
    let mut current_inside_brackets = false;

    let chars: Vec<_> = ip.chars().collect();
    for (i, c) in chars.iter().enumerate() {
        if *c == '[' {
            current_inside_brackets = true;
            continue;
        } else if *c == ']' {
            current_inside_brackets = false;
            continue;
        }

        if i + 2 >= ip.len() {
            break;
        }

        if chars[i + 2] == *c && chars[i + 1] != *c {
            let found = String::from_iter(&chars[i..=i + 2]);

            if current_inside_brackets {
                bab_registry.insert(found, true);
            } else {
                aba_registry.insert(found, true);
            }
        }
    }

    for (key, _) in aba_registry {
        let key_chars: Vec<_> = key.chars().collect();
        let bab = String::from_iter(vec![key_chars[1], key_chars[0], key_chars[1]]);

        match bab_registry.get(bab.as_str()) {
            Some(_) => return true,
            None => continue,
        }
    }

    false
}
