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
        let mut total = 0;

        for mut range in self
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
        if number % i == 0 {
            divisors.push(i);

            if i * i != number {
                divisors.push(number / i);
            }
        }
    }

    divisors.sort();
    divisors
}
