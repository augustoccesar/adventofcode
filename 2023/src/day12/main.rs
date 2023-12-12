use std::thread;

use aoc2023::{read_input, read_named_input, timed};
use regex::Regex;

fn part_one() -> String {
    let rows: Vec<(String, Vec<i32>)> = read_input("12")
        .lines()
        .map(|line| {
            let parts = line.split(' ').collect::<Vec<&str>>();
            let format = parts[1]
                .split(',')
                .map(|item| item.parse::<i32>().unwrap())
                .collect::<Vec<i32>>();

            (parts[0].to_string(), format)
        })
        .collect();

    let mut total_count = 0;
    let (tx, rx) = std::sync::mpsc::channel::<u64>();

    for row in &rows {
        let packed_size: i32 = row.1.iter().sum::<i32>() + row.1.len() as i32 - 1;
        let mut packed_version: Vec<char> = vec!['.'; packed_size as usize];

        let mut current_pos = 0;
        for format_group_size in &row.1 {
            for _ in 0..*format_group_size {
                packed_version[current_pos] = '#';

                current_pos += 1;
            }

            current_pos += 1;
        }

        let tx_copy = tx.clone();
        let row_copy = row.0.clone();
        thread::spawn(move || {
            let compact_row = compact_row(&row_copy.chars().collect::<Vec<char>>());
            let count = find_options(&compact_row, &packed_version);

            tx_copy.send(count).unwrap();
        });
    }

    for _ in 0..rows.len() {
        total_count += rx.recv().unwrap();
    }

    total_count.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn compact_row(row: &[char]) -> Vec<char> {
    let re = Regex::new(r"\.+").unwrap();
    re.replace_all(&row.iter().collect::<String>(), ".")
        .trim_matches('.')
        .chars()
        .collect::<Vec<char>>()
}

fn find_options(compacted_row: &[char], compacted_format: &[char]) -> u64 {
    for (i, c) in compacted_row.iter().enumerate() {
        match c {
            '?' => {
                let mut option_1 = vec!['.'; compacted_row.len()];
                option_1.copy_from_slice(compacted_row);
                option_1[i] = '.';
                let option_1 = compact_row(&option_1);

                let mut option_2 = vec!['.'; compacted_row.len()];
                option_2.copy_from_slice(compacted_row);
                option_2[i] = '#';
                let option_2 = compact_row(&option_2);

                let mut count = 0_u64;
                count += find_options(&option_1, compacted_format);
                count += find_options(&option_2, compacted_format);
                return count;
            }
            '.' | '#' => {
                if i > compacted_format.len() - 1 {
                    continue;
                }

                if compacted_row[i] != compacted_format[i] {
                    return 0;
                }
            }
            _ => unreachable!(),
        }
    }

    if compacted_row == compacted_format {
        1
    } else {
        0
    }
}
