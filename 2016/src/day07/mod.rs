use crate::task::Task;

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
                false => contains_outside_brackets = true
            }
        }
    }

    return contains_outside_brackets && !contains_inside_brackets;
}
