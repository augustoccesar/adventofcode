use crate::Day;

pub struct Day06 {}

impl Day for Day06 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        6
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();
        let lines = input.lines().rev().collect::<Vec<&str>>();
        let operations = lines[0]
            .split_whitespace()
            .map(|char_str| {
                char_str
                    .chars()
                    .next()
                    .expect("each operation should be a single character")
            })
            .collect::<Vec<char>>();

        let mut total = vec![0; operations.len()];
        for (i, operation) in operations.iter().enumerate() {
            match operation {
                '+' => total[i] = 0,
                '*' => total[i] = 1,
                _ => panic!("invalid operation character"),
            }
        }

        for line in &lines[1..] {
            let numbers = line
                .split_whitespace()
                .map(|num_str| {
                    num_str
                        .parse::<i64>()
                        .expect("each item on a line should be a valid i64")
                })
                .collect::<Vec<i64>>();

            assert!(numbers.len() == total.len());

            for i in 0..total.len() {
                match operations[i] {
                    '+' => total[i] += numbers[i],
                    '*' => total[i] *= numbers[i],
                    _ => panic!("invalid operation character"),
                }
            }
        }

        total.iter().sum::<i64>().to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}
