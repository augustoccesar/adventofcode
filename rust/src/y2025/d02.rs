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

        for (start, end) in parse_input(&self.read_default_input()) {
            if int_len(start) == int_len(end) && !int_len(start).is_multiple_of(2) {
                continue;
            }

            for number in start..=end {
                let number_digits = number.to_string().chars().collect::<Vec<char>>();

                if number_digits.len() % 2 != 0 {
                    continue;
                }

                let half_idx = number_digits.len() / 2;

                if number_digits[0..half_idx] == number_digits[half_idx..] {
                    total += number;
                }
            }
        }

        total.to_string()
    }

    fn part_two(&self) -> String {
        let mut total = 0;

        for (start, end) in parse_input(&self.read_default_input()) {
            'number_loop: for number in start..=end {
                let number_digits = number.to_string().chars().collect::<Vec<char>>();

                if number_digits.len() < 2 {
                    continue;
                }

                if is_all_same(&number_digits) {
                    total += number;
                    continue;
                }

                for divisor in divisors(number_digits.len()) {
                    let chunk_size = number_digits.len() / divisor;

                    let groups = number_digits
                        .chunks(chunk_size)
                        .map(|chars| chars.iter().collect::<String>())
                        .collect::<Vec<String>>();

                    if is_all_same(&groups) {
                        total += number;

                        continue 'number_loop;
                    }
                }
            }
        }

        total.to_string()
    }
}

fn parse_input(input: &str) -> Vec<(i64, i64)> {
    input
        .split(",")
        .map(|range| range.split("-").map(|side| side.parse::<i64>().unwrap()))
        .map(|mut range| {
            (
                range
                    .next()
                    .expect("range to have at two parts. Missing first."),
                range
                    .next()
                    .expect("range to have at two parts. Missing second."),
            )
        })
        .collect::<_>()
}

fn int_len(integer: i64) -> usize {
    integer.to_string().len()
}

fn is_all_same<T>(arr: &[T]) -> bool
where
    T: PartialEq,
{
    if arr.is_empty() {
        return true;
    }

    let first = &arr[0];
    arr.iter().all(|item| item == first)
}

fn divisors(number: usize) -> Vec<usize> {
    let mut divisors = Vec::new();
    let limit = (number as f64).sqrt() as usize;

    // By skipping the '1' it avoids adding 1 and the number itself.
    // Since when we are grouping we don't want to group by the number and also
    // we the groups must be bigger than 2, so this just those cases.
    for i in 2..=limit {
        if number.is_multiple_of(i) {
            divisors.push(i);

            if i * i != number {
                divisors.push(number / i);
            }
        }
    }

    divisors.sort();
    divisors
}
