use crate::Day;

pub struct Day03 {}

impl Day for Day03 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        3
    }

    fn part_one(&self) -> String {
        let banks = self
            .read_default_input()
            .lines()
            .map(|line| line.chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>();

        let mut result = 0;
        for bank in banks {
            let mut largest = 0;

            for i in 0..bank.len() {
                for j in i + 1..bank.len() {
                    let value = [bank[i], bank[j]]
                        .iter()
                        .collect::<String>()
                        .parse::<i64>()
                        .unwrap();

                    if value > largest {
                        largest = value;
                    }
                }
            }

            result += largest;
        }

        result.to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}
