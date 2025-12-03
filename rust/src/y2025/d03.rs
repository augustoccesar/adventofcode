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
            .map(|line| line.chars().map(char_to_digit).collect::<Vec<_>>())
            .collect::<Vec<Vec<u8>>>();

        let mut result: i64 = 0;
        for bank in banks {
            let mut largest = 0;

            for i in 0..bank.len() {
                for j in i + 1..bank.len() {
                    let value = bank[i] * 10 + bank[j];

                    if value > largest {
                        largest = value;
                    }
                }
            }

            result += largest as i64;
        }

        result.to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}

fn char_to_digit(c: char) -> u8 {
    c.to_digit(10).unwrap() as u8
}
