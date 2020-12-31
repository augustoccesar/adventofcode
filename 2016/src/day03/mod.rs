use crate::task::Task;
use regex::Regex;

pub struct Day03 {}

impl Task for Day03 {
    fn day(&self) -> std::string::String {
        return String::from("03");
    }

    fn part_one(&self) {
        let input = self.read_input();
        let re = Regex::new(r"(\d+)\s+(\d+)\s+(\d+)").unwrap();

        let mut valid = 0;

        for cap in re.captures_iter(&input) {
            let a = cap[1].parse::<i16>();
            let b = cap[2].parse::<i16>();
            let c = cap[3].parse::<i16>();

            if is_valid([a.unwrap(), b.unwrap(), c.unwrap()]) {
                valid += 1;
            }
        }

        println!("Part One: {}", valid);
    }
    fn part_two(&self) {
        println!("Part Two");
    }
}

// --------------------------------------------------------------------------------------------------------------------

fn is_valid(triangle: [i16; 3]) -> bool {
    let (a, b, c) = (triangle[0], triangle[1], triangle[2]);
    if a + b <= c || a + c <= b || b + c <= a {
        return false;
    }

    return true;
}
