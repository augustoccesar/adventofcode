use crate::task::Task;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref TRIANGLE_RE: Regex = Regex::new(r"(\d+)\s+(\d+)\s+(\d+)").unwrap();
}

pub struct Day03 {}

impl Task for Day03 {
    fn day(&self) -> std::string::String {
        return String::from("03");
    }

    fn part_one(&self) {
        let input = self.read_input();

        let mut valid = 0;

        for cap in TRIANGLE_RE.captures_iter(&input) {
            let a = cap[1].parse::<i16>();
            let b = cap[2].parse::<i16>();
            let c = cap[3].parse::<i16>();

            if is_valid(&[a.unwrap(), b.unwrap(), c.unwrap()]) {
                valid += 1;
            }
        }

        println!("Part One: {}", valid);
    }
    fn part_two(&self) {
        // 0 1 2
        // 3 4 5
        // 6 7 8
        // reset
        // ...

        let input = self.read_input();

        let mut rows: Vec<[i16; 3]> = vec![];
        for cap in TRIANGLE_RE.captures_iter(&input) {
            let a = cap[1].parse::<i16>();
            let b = cap[2].parse::<i16>();
            let c = cap[3].parse::<i16>();
            rows.push([a.unwrap(), b.unwrap(), c.unwrap()])
        }

        let mut valid = 0;
        let mut pivot = 0;
        let mut curr_batch = [[0; 3], [0; 3], [0; 3]];

        for i in 0..rows.len() {
            let row = rows[i];
            for j in 0..row.len() {
                let idx = pivot / 3;
                let triangle = pivot % 3;

                curr_batch[triangle][idx] = row[j];
                pivot += 1;
            }

            if pivot == 9 {
                for triangle in &curr_batch {
                    if is_valid(triangle) {
                        valid += 1;
                    }
                }

                pivot = 0;
                curr_batch = [[0; 3], [0; 3], [0; 3]];
            }
        }

        println!("Part Two: {}", valid);
    }
}

// --------------------------------------------------------------------------------------------------------------------

fn is_valid(triangle: &[i16; 3]) -> bool {
    let (a, b, c) = (triangle[0], triangle[1], triangle[2]);
    if a + b <= c || a + c <= b || b + c <= a {
        return false;
    }

    return true;
}
