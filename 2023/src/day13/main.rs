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
        match summarize_rows(pattern) {
            0 => summary += summarize_cols(pattern),
            rows => summary += rows * 100,
        }
    }

    summary.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn summarize_rows(pattern: &[Vec<char>]) -> usize {
    let mut fixed_smudge = false;

    for y in 0..pattern.len() - 1 {
        if pattern[y] == pattern[y + 1] {
            let mut mirrored = true;
            let mut inner_y1 = y as i32 - 1;
            let mut inner_y2 = (y + 1) as i32 + 1;

            while inner_y1 >= 0 && inner_y2 < pattern.len() as i32 {
                if pattern[inner_y1 as usize] != pattern[inner_y2 as usize] {
                    if !fixed_smudge
                        && has_single_diff(&pattern[inner_y1 as usize], &pattern[inner_y2 as usize])
                    {
                        fixed_smudge = true;
                    } else {
                        mirrored = false;
                        fixed_smudge = false;
                        break;
                    }
                }

                inner_y1 -= 1;
                inner_y2 += 1;
            }

            if mirrored && fixed_smudge {
                return y + 1;
            }
        } else if !fixed_smudge && has_single_diff(&pattern[y], &pattern[y + 1]) {
            let mut mirrored = true;
            let mut inner_y1 = y as i32 - 1;
            let mut inner_y2 = (y + 1) as i32 + 1;

            while inner_y1 >= 0 && inner_y2 < pattern.len() as i32 {
                if pattern[inner_y1 as usize] != pattern[inner_y2 as usize] {
                    mirrored = false;
                    break;
                }

                inner_y1 -= 1;
                inner_y2 += 1;
            }

            if mirrored {
                return y + 1;
            }
        }
    }

    0
}

fn summarize_cols(pattern: &[Vec<char>]) -> usize {
    let rotated_pattern = rotate_pattern(pattern);

    summarize_rows(&rotated_pattern)
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

fn has_single_diff(a: &Vec<char>, b: &Vec<char>) -> bool {
    if a == b {
        return false;
    }

    let mut has_diff = false;
    for i in 0..a.len() {
        if a[i] != b[i] {
            if has_diff {
                return false;
            } else {
                has_diff = true;
            }
        }
    }

    has_diff
}
