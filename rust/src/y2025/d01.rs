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
        let input = input.lines().map(|line| {
            let chars = line.chars().collect::<Vec<_>>();

            let number = chars[1..]
                .iter()
                .copied()
                .collect::<String>()
                .parse::<i32>()
                .expect("Invalid format of line");

            let number = number % 100;

            match &chars[0] {
                'R' => number,
                'L' => number * -1,
                _ => unreachable!("Invalid format of line"),
            }
        });

        let mut total = 0;
        let mut position = 50;

        for rotation in input {
            position += rotation;

            if position < 0 {
                position = 100 + position;
            } else if position > 99 {
                position %= 100;
            }

            if position == 0 {
                total += 1;
            }
        }

        total.to_string()
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();
        let input = input.lines().map(|line| {
            let chars = line.chars().collect::<Vec<_>>();

            let number = chars[1..]
                .iter()
                .copied()
                .collect::<String>()
                .parse::<i32>()
                .expect("Invalid format of line");

            match &chars[0] {
                'R' => number,
                'L' => number * -1,
                _ => unreachable!("Invalid format of line"),
            }
        });

        let mut clicks = 0;
        let mut position = 50;

        // TODO: There must be a better way xD
        for rotation in input {
            let direction = if rotation > 0 { 1 } else { -1 };
            for _ in 0..rotation.abs() {
                position += direction;

                if position == -1 {
                    position = 99;
                } else if position == 100 {
                    position = 0;
                }

                if position == 0 {
                    clicks += 1;
                }
            }
        }

        clicks.to_string()
    }
}
