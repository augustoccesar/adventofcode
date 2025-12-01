use crate::Day;

pub struct Day01 {}

impl Day for Day01 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        1
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();
        let mut safe = Safe::new(PasswordMethod::Default);

        let mut result = 0;
        for line in input.lines() {
            result += safe.rotate(line);
        }

        result.to_string()
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();
        let mut safe = Safe::new(PasswordMethod::X434C49434B);

        let mut result = 0;
        for line in input.lines() {
            result += safe.rotate(line);
        }

        result.to_string()
    }
}

#[derive(PartialEq)]
enum PasswordMethod {
    Default,
    X434C49434B,
}

struct Safe {
    dial: i32,
    method: PasswordMethod,
}

impl Safe {
    pub fn new(method: PasswordMethod) -> Self {
        Self { dial: 50, method }
    }

    pub fn rotate(&mut self, input: &str) -> i32 {
        let mut clicks = 0;

        let chars = input.chars().collect::<Vec<_>>();
        let direction = match chars[0] {
            'R' => 1,
            'L' => -1,
            _ => unreachable!("Invalid input format"),
        };

        let rotation = chars[1..]
            .iter()
            .copied()
            .collect::<String>()
            .parse::<i32>()
            .expect("Invalid format of line");

        // TODO[2025-12-01]: There must be a better way xD
        for _ in 0..rotation.abs() {
            self.dial += direction;

            if self.dial == -1 {
                self.dial = 99;
            } else if self.dial == 100 {
                self.dial = 0;
            }

            // 0x434C49434B method increments everytime it passes through 0
            if self.method == PasswordMethod::X434C49434B && self.dial == 0 {
                clicks += 1;
            }
        }

        // Default method only increments if the dial stops at 0
        if self.method == PasswordMethod::Default && self.dial == 0 {
            clicks += 1;
        }

        clicks
    }
}
