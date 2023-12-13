use aoc2023::{read_input, timed};

fn part_one() -> String {
    let patterns = read_input("13")
        .split("\n\n")
        .map(|pattern| {
            pattern
                .lines()
                .map(|line| line.chars().collect::<Vec<char>>())
                .collect::<Vec<Vec<char>>>()
        })
        .collect::<Vec<Vec<Vec<char>>>>();

    let mut summary = 0;
    for pattern in &patterns {
        match summarize_rows(pattern, false) {
            0 => summary += summarize_cols(pattern, false),
            rows => summary += rows * 100,
        }
    }

    summary.to_string()
}

fn part_two() -> String {
    let patterns = read_input("13")
        .split("\n\n")
        .map(|pattern| {
            pattern
                .lines()
                .map(|line| line.chars().collect::<Vec<char>>())
                .collect::<Vec<Vec<char>>>()
        })
        .collect::<Vec<Vec<Vec<char>>>>();

    let mut summary = 0;
    for pattern in &patterns {
        match summarize_rows(pattern, true) {
            0 => summary += summarize_cols(pattern, true),
            rows => summary += rows * 100,
        }
    }

    summary.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn summarize_rows(pattern: &[Vec<char>], allowed_smudges: bool) -> usize {
    for y in 0..pattern.len() - 1 {
        let mut difference = diff(&pattern[y], &pattern[y + 1]);

        match (difference, allowed_smudges) {
            (0, _) => (),
            (_, true) => (),
            _ => continue,
        }

        let mut inner_y1 = y as i32 - 1;
        let mut inner_y2 = (y + 1) as i32 + 1;

        while inner_y1 >= 0 && inner_y2 < pattern.len() as i32 {
            difference += diff(&pattern[inner_y1 as usize], &pattern[inner_y2 as usize]);

            inner_y1 -= 1;
            inner_y2 += 1;
        }

        match (allowed_smudges, difference) {
            (false, 0) => return y + 1,
            (true, d) if d > 0 && d < 2 => return y + 1,
            _ => continue,
        }
    }

    0
}

fn summarize_cols(pattern: &[Vec<char>], allowed_smudges: bool) -> usize {
    let rotated_pattern = rotate_pattern(pattern);

    summarize_rows(&rotated_pattern, allowed_smudges)
}

fn rotate_pattern(pattern: &[Vec<char>]) -> Vec<Vec<char>> {
    let col_size = pattern[0].len();
    let row_size = pattern.len();

    let mut rotated_pattern = vec![vec!['.'; row_size]; col_size];

    for y in 0..pattern.len() {
        for x in 0..pattern[0].len() {
            rotated_pattern[x][y] = pattern[y][x];
        }
    }

    rotated_pattern
}

fn diff(a: &Vec<char>, b: &Vec<char>) -> u8 {
    if a == b {
        return 0;
    }

    let mut has_diff = false;
    for i in 0..a.len() {
        if a[i] != b[i] {
            if has_diff {
                return 2;
            } else {
                has_diff = true;
            }
        }
    }

    1
}
