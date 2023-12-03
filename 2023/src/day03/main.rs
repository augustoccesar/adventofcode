use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    let matrix: Vec<Vec<char>> = read_input("03")
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

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn has_adjacent_symbol(matrix: &Vec<Vec<char>>, x: usize, y: usize) -> bool {
    let modifiers = [
        (0, -1),  // TOP
        (1, -1),  // TOP RIGHT
        (1, 0),   // RIGHT
        (1, 1),   // BOTTOM RIGHT
        (0, 1),   // BOTTOM
        (-1, -1), // BOTTOM LEFT
        (-1, 0),  // LEFT
        (-1, 1),  // TOP LEFT
    ];

    for (x_mod, y_mod) in modifiers {
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
