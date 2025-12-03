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
                    let value = concatenate_digits(bank[i], bank[j]);

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

fn concatenate_digits(a: u8, b: u8) -> u8 {
    if a > 9 || b > 9 {
        panic!("input must be single digit")
    }

    a * 10 + b
}

#[cfg(test)]
mod tests {
    use crate::y2025::d03::concatenate_digits;

    #[test]
    fn test_concatenate_digits() {
        assert_eq!(98, concatenate_digits(9, 8));
        assert_eq!(89, concatenate_digits(8, 9));
        assert_eq!(10, concatenate_digits(1, 0));
        assert_eq!(1, concatenate_digits(0, 1));
    }
}
