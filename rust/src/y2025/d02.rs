use crate::Day;

pub struct Day02 {}

impl Day for Day02 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        2
    }

    fn part_one(&self) -> String {
        let mut total = 0;

        for mut range in self
            // .read_input("example")
            .read_default_input()
            .split(",")
            .map(|range| range.split("-"))
        {
            let start = range
                .next()
                .expect("range to have at two parts. Missing first.")
                .parse::<i64>()
                .unwrap();
            let end = range
                .next()
                .expect("range to have at two parts. Missing second.")
                .parse::<i64>()
                .unwrap();

            if int_len(start) == int_len(end) && int_len(start) % 2 != 0 {
                continue;
            }

            'a: for number in start..=end {
                let number_digits = number.to_string().chars().collect::<Vec<char>>();

                if number_digits.len() % 2 != 0 {
                    continue;
                }

                // TODO: Don't think I even need this. Just split the string and compare the two halves
                let mut i = 0;
                let mut j = number_digits.len() / 2;

                for _ in 0..number_digits.len() / 2 {
                    if number_digits[i] == number_digits[j] {
                        i += 1;
                        j += 1;

                        continue;
                    } else {
                        continue 'a;
                    }
                }

                total += number;
            }
        }

        total.to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}

fn int_len(integer: i64) -> usize {
    integer.to_string().len()
}
