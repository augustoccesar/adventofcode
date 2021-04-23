use crate::task::Task;
use regex::Regex;

pub struct Day07 {}

impl Task for Day07 {
    fn day(&self) -> String {
        return String::from("07");
    }

    fn part_one(&self) {
        let input = self.read_input();

        let mut count = 0;
        for ip in input.lines() {
            if support_tls(ip) {
                count += 1;
            }
        }

        println!("Part One: {}", count.to_string());
    }
    fn part_two(&self) {
        println!("Part Two: {}", "-");
    }
}

// --------------------------------------------------------------------------------------------------------------------

// TODO: Rethink this solution. This is extremelly unoptimized. Should be able to do with a single scan of the ip,
//       and keeping track if is currently inside brackets
fn support_tls(ip: &str) -> bool {
    let brackets_reg = Regex::new(r"\[(\w+)\]").unwrap();
    let mut contains_inside_brackets = false;
    let mut contains_outside_brackets = false;

    let mut in_brackets = vec!();
    for item in brackets_reg.captures_iter(ip) {
        in_brackets.push(String::from(&item[1]));
    }

    for item in in_brackets {
        let item_chars: Vec<_> = item.chars().collect();
        for (i, c) in item_chars.iter().enumerate() {
            if i+1 >= item_chars.len() || i+2 >= item_chars.len() || i+3 >= item_chars.len() {
                break;
            }
            if (item_chars[i + 3] == *c && item_chars[i + 1] == item_chars[i + 2]) && item_chars[i+1] != *c{
                contains_inside_brackets = true;
            }
        }
    }

    let out_of_brackets = brackets_reg.replace_all(ip, ";");
    for item in out_of_brackets.split(";") {
        let item_chars: Vec<_> = item.chars().collect();
        for (i, c) in item_chars.iter().enumerate() {
            if i+1 >= item_chars.len() || i+2 >= item_chars.len() || i+3 >= item_chars.len() {
                break;
            }
            if (item_chars[i + 3] == *c && item_chars[i + 1] == item_chars[i + 2]) && item_chars[i+1] != *c {
                contains_outside_brackets = true;
            }
        }
    }

    return contains_outside_brackets && !contains_inside_brackets;
}
