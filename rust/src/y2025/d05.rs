use std::{cmp, ops::RangeInclusive};

use crate::Day;

pub struct Day05 {}

impl Day for Day05 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        5
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();
        let mut count = 0;

        let mut ranges = Vec::<RangeInclusive<u64>>::new();

        let mut database = input.split("\n\n");
        for line in database.next().unwrap().lines() {
            let mut fresh_range = line
                .split("-")
                .map(|ingredient| ingredient.parse::<u64>().unwrap());

            ranges.push(RangeInclusive::new(
                fresh_range.next().unwrap(),
                fresh_range.next().unwrap(),
            ));
        }

        for ingredient in database
            .next()
            .unwrap()
            .lines()
            .map(|line| line.parse::<u64>().unwrap())
        {
            for range in &ranges {
                if range.contains(&ingredient) {
                    count += 1;
                    break;
                }
            }
        }

        count.to_string()
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();

        type Range = (i64, i64);

        fn overlapping(a: &Range, b: &Range) -> i64 {
            cmp::max(0, cmp::min(a.1, b.1) - cmp::max(a.0, b.0) + 1)
        }

        let mut ranges: Vec<Range> = Vec::new();
        for line in input.split("\n\n").next().unwrap().lines() {
            let fresh_range = line
                .split("-")
                .map(|ingredient| {
                    ingredient
                        .parse::<i64>()
                        .expect("range parts to be valid i64")
                })
                .collect::<Vec<i64>>();

            assert!(fresh_range.len() == 2, "invalid range format");

            ranges.push((fresh_range[0], fresh_range[1]));
        }

        loop {
            let mut to_add: Vec<Range> = Vec::new();
            let mut to_remove: Vec<usize> = Vec::new();

            // Any time there is a change to the `ranges`, we start again. Not optimal, but ¯\_(ツ)_/¯.
            // TODO[2025-12-05]: Look at a better way to do this.
            'outer: for i in 0..ranges.len() {
                for j in 0..ranges.len() {
                    if i == j {
                        continue;
                    };

                    // If ranges[i] fully includes ranges[j], remove `j` as it is unnecessary
                    if ranges[i].0 <= ranges[j].0 && ranges[i].1 >= ranges[j].1 {
                        to_remove.push(j);

                        break 'outer;
                    }

                    let overlapping = overlapping(&ranges[i], &ranges[j]);
                    if overlapping > 0 {
                        to_add.push((ranges[i].0, ranges[j].0 - 1));
                        to_add.push((ranges[j].0, ranges[i].1));
                        to_add.push((ranges[i].1 + 1, ranges[j].1));

                        to_remove.push(i);
                        to_remove.push(j);

                        break 'outer;
                    }
                }
            }

            // No more action to take. The ranges should be unique by now.
            if to_add.is_empty() && to_remove.is_empty() {
                break;
            }

            to_remove.sort_by_key(|item| cmp::Reverse(*item));
            to_remove.sort_by(|a, b| b.cmp(a));
            for remove_idx in to_remove {
                ranges.remove(remove_idx);
            }

            for new_range in to_add {
                ranges.push(new_range);
            }
        }

        ranges
            .iter()
            .map(|range| (range.1 - range.0) + 1)
            .sum::<i64>()
            .to_string()
    }
}
