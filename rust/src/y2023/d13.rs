use crate::Day;

pub struct Day13 {}

impl Day for Day13 {
    fn year(&self) -> u16 {
        2023
    }

    fn day(&self) -> u8 {
        13
    }

    fn part_one(&self) -> String {
        let patterns = parse_input(&self.read_default_input());
        summarize(&patterns, false).to_string()
    }

    fn part_two(&self) -> String {
        let patterns = parse_input(&self.read_default_input());
        summarize(&patterns, true).to_string()
    }
}

fn parse_input(input: &str) -> Vec<Vec<Vec<char>>> {
    input
        .split("\n\n")
        .map(|pattern| {
            pattern
                .lines()
                .map(|line| line.chars().collect::<Vec<char>>())
                .collect::<Vec<Vec<char>>>()
        })
        .collect::<Vec<Vec<Vec<char>>>>()
}

fn summarize(patterns: &[Vec<Vec<char>>], allow_smudges: bool) -> usize {
    let mut summary = 0;
    for pattern in patterns {
        match summarize_rows(pattern, allow_smudges) {
            0 => summary += summarize_cols(pattern, allow_smudges),
            rows => summary += rows * 100,
        }
    }

    summary
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
            (true, 1) => return y + 1,
            _ => continue,
        }
    }

    0
}

fn summarize_cols(pattern: &[Vec<char>], allowed_smudges: bool) -> usize {
    let rotated_pattern = rotate_pattern(pattern);

    summarize_rows(&rotated_pattern, allowed_smudges)
}

#[allow(clippy::needless_range_loop)]
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
