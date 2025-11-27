use std::collections::HashMap;

use crate::Day;

pub struct Day03 {}

impl Day for Day03 {
    fn year(&self) -> u16 {
        2023
    }

    fn day(&self) -> u8 {
        3
    }

    fn part_one(&self) -> String {
        let matrix: Vec<Vec<char>> = self
            .read_default_input()
            .lines()
            .map(|line| line.chars().collect())
            .collect();
        let mut numbers: Vec<i64> = vec![];

        for y in 0..matrix.len() {
            let mut x = 0;
            while x < matrix[y].len() {
                let item = matrix[y][x];

                if item.is_ascii_digit() {
                    let mut adjacent_to_symbol = has_adjacent_symbol(&matrix, x, y);

                    let mut digit = vec![item];
                    let mut next = x + 1;
                    if next >= matrix[y].len() {
                        x = next;
                        continue;
                    }

                    while next < matrix[y].len() && matrix[y][next].is_ascii_digit() {
                        if has_adjacent_symbol(&matrix, next, y) {
                            adjacent_to_symbol = true;
                        }

                        digit.push(matrix[y][next]);
                        next += 1;
                    }

                    x = next;

                    if adjacent_to_symbol {
                        numbers.push(
                            digit
                                .into_iter()
                                .collect::<String>()
                                .parse::<i64>()
                                .unwrap(),
                        );
                    }

                    continue;
                }

                x += 1;
            }
        }

        numbers.iter().sum::<i64>().to_string()
    }

    fn part_two(&self) -> String {
        let matrix: Vec<Vec<char>> = self
            .read_default_input()
            .lines()
            .map(|line| line.chars().collect())
            .collect();
        let mut gears: HashMap<(usize, usize), Vec<i64>> = HashMap::new();

        for y in 0..matrix.len() {
            let mut x = 0;
            while x < matrix[y].len() {
                let item = matrix[y][x];

                if item.is_ascii_digit() {
                    let mut gear = adjacent_gear(&matrix, x, y);

                    let mut digit = vec![item];
                    let mut next = x + 1;

                    while next < matrix[y].len() && matrix[y][next].is_ascii_digit() {
                        if gear.is_none() {
                            gear = adjacent_gear(&matrix, next, y);
                        }

                        digit.push(matrix[y][next]);
                        next += 1;
                    }

                    x = next;

                    if let Some(gear_pos) = gear {
                        gears.entry(gear_pos).or_default();

                        let number = digit
                            .into_iter()
                            .collect::<String>()
                            .parse::<i64>()
                            .unwrap();

                        gears.get_mut(&gear_pos).unwrap().push(number);
                    }

                    continue;
                }

                x += 1;
            }
        }

        gears
            .iter()
            .filter(|it| it.1.len() == 2)
            .map(|it| it.1.iter().copied().reduce(|acc, e| acc * e).unwrap())
            .sum::<i64>()
            .to_string()
    }
}

const POS_MODIFIERS: [(i32, i32); 8] = [
    (0, -1),  // TOP
    (1, -1),  // TOP RIGHT
    (1, 0),   // RIGHT
    (1, 1),   // BOTTOM RIGHT
    (0, 1),   // BOTTOM
    (-1, -1), // BOTTOM LEFT
    (-1, 0),  // LEFT
    (-1, 1),  // TOP LEFT
];

fn has_adjacent_symbol(matrix: &[Vec<char>], x: usize, y: usize) -> bool {
    for (x_mod, y_mod) in POS_MODIFIERS {
        let x = x as i32 + x_mod;
        let y = y as i32 + y_mod;

        if x < 0 || y < 0 {
            continue;
        }

        let x = x as usize;
        let y = y as usize;

        if y >= matrix.len() || x >= matrix[y].len() {
            continue;
        }

        let item = matrix[y][x];
        if !item.is_ascii_digit() && item != '.' {
            return true;
        }
    }

    false
}

fn adjacent_gear(matrix: &[Vec<char>], x: usize, y: usize) -> Option<(usize, usize)> {
    for (x_mod, y_mod) in POS_MODIFIERS {
        let x = x as i32 + x_mod;
        let y = y as i32 + y_mod;

        if x < 0 || y < 0 {
            continue;
        }

        let x = x as usize;
        let y = y as usize;

        if y >= matrix.len() || x >= matrix[y].len() {
            continue;
        }

        let item = matrix[y][x];
        if item == '*' {
            return Some((x, y));
        }
    }

    None
}
