use std::{collections::HashSet, option, thread};

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
            let compact_row = compact_row(&row_copy);
            let count = find_options(
                &compact_row.chars().collect::<Vec<char>>(),
                &packed_version.iter().collect::<String>(),
            );

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

fn compact_row(row: &str) -> String {
    let re = Regex::new(r"\.+").unwrap();
    re.replace_all(row, ".").trim_matches('.').to_string()
}

fn find_options(row: &[char], compact_format: &str) -> u64 {
    let mut count = 0_u64;

    for (i, c) in row.iter().enumerate() {
        if c == &'?' {
            let mut option_1 = vec!['.'; row.len()];
            option_1.copy_from_slice(row);
            option_1[i] = '.';

            let mut option_2 = vec!['.'; row.len()];
            option_2.copy_from_slice(row);
            option_2[i] = '#';

            count += find_options(&option_1, compact_format);
            count += find_options(&option_2, compact_format);
            return count;
        }
    }

    let compact = compact_row(&row.iter().collect::<String>());
    if compact == compact_format {
        count += 1;
    }

    count
}
