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
            .map(|operation_char| match operation_char {
                '*' => Operation::Mult,
                '+' => Operation::Add,
                _ => panic!("invalid operation character"),
            })
            .collect::<Vec<Operation>>();

        let mut total = vec![0; operations.len()];
        for (i, operation) in operations.iter().enumerate() {
            match operation {
                Operation::Add => total[i] = 0,
                Operation::Mult => total[i] = 1,
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
                    Operation::Add => total[i] += numbers[i],
                    Operation::Mult => total[i] *= numbers[i],
                }
            }
        }

        total.iter().sum::<i64>().to_string()
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();
        let lines = input.lines().collect::<Vec<&str>>();

        let mut data: Vec<Vec<char>> = vec![vec![]; lines[0].len()];

        for line in &lines {
            for (i, character) in line.chars().enumerate() {
                data[i].push(character);
            }
        }

        let mut total = 0;
        let mut curr_problem_total = 0;
        let mut curr_operation = None;
        for scan in data {
            // If the last char in the vertical scan is * or +, a new problem is starting.
            match scan.last().expect("scan should not be empty") {
                '*' => {
                    curr_operation = Some(Operation::Mult);
                    curr_problem_total = 1;
                }
                '+' => {
                    curr_operation = Some(Operation::Add);
                    curr_problem_total = 0;
                }
                _ => (),
            }

            let mut digits = scan
                .iter()
                .filter(|c| **c != ' ' && **c != '*' && **c != '+')
                .map(|c| {
                    c.to_digit(10)
                        .expect("unfiltered characters should be a valid digit")
                        as i64
                })
                .rev()
                .peekable();

            // The only vertical scan without any digits is the one before the start of a new
            // problem, so add the total of the current problem to the aggregate total and
            // move to the next vertical scan.
            if let None = digits.peek() {
                total += curr_problem_total;

                continue;
            }

            let mut number = 0i64;
            for (i, digit) in digits.enumerate() {
                number += digit * (10i64.pow(i as u32));
            }

            match curr_operation.clone().unwrap() {
                Operation::Add => curr_problem_total += number,
                Operation::Mult => curr_problem_total *= number,
            }
        }

        total += curr_problem_total;

        total.to_string()
    }
}

#[derive(Debug, Clone)]
enum Operation {
    Add,
    Mult,
}
