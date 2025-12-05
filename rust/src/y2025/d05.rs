use std::cmp;

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
        let database = Database::parse(&self.read_default_input());
        let mut count = 0;

        for ingredient in &database.ingredients {
            for range in &database.fresh_ranges {
                if *ingredient >= range.0 && *ingredient <= range.1 {
                    count += 1;

                    break;
                }
            }
        }

        count.to_string()
    }

    fn part_two(&self) -> String {
        let mut database = Database::parse(&self.read_default_input());

        loop {
            let mut to_add: Vec<Range> = Vec::new();
            let mut to_remove: Vec<usize> = Vec::new();

            // Any time there is a change to the `ranges`, we start again. Not optimal, but ¯\_(ツ)_/¯.
            // TODO[2025-12-05]: Look at a better way to do this.
            'outer: for i in 0..database.fresh_ranges.len() {
                for j in 0..database.fresh_ranges.len() {
                    if i == j {
                        continue;
                    };

                    // If ranges[i] fully includes ranges[j], remove `j` as it is unnecessary
                    if database.fresh_ranges[i].0 <= database.fresh_ranges[j].0
                        && database.fresh_ranges[i].1 >= database.fresh_ranges[j].1
                    {
                        to_remove.push(j);

                        break 'outer;
                    }

                    if overlapping(&database.fresh_ranges[i], &database.fresh_ranges[j]) > 0 {
                        to_add.push((database.fresh_ranges[i].0, database.fresh_ranges[j].0 - 1));
                        to_add.push((database.fresh_ranges[j].0, database.fresh_ranges[i].1));
                        to_add.push((database.fresh_ranges[i].1 + 1, database.fresh_ranges[j].1));

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
                database.fresh_ranges.remove(remove_idx);
            }

            for new_range in to_add {
                database.fresh_ranges.push(new_range);
            }
        }

        database
            .fresh_ranges
            .iter()
            .map(|range| (range.1 - range.0) + 1)
            .sum::<i64>()
            .to_string()
    }
}

type Range = (i64, i64);
type Ingredient = i64;

struct Database {
    fresh_ranges: Vec<Range>,
    ingredients: Vec<Ingredient>,
}

impl Database {
    pub fn parse(input: &str) -> Self {
        let input_parts = input.split("\n\n").collect::<Vec<&str>>();
        assert!(input_parts.len() == 2);

        let mut ranges: Vec<Range> = Vec::new();
        for line in input_parts[0].lines() {
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

        let ingredients = input_parts[1]
            .lines()
            .map(|line| line.parse::<i64>().unwrap())
            .collect::<Vec<_>>();

        Self {
            fresh_ranges: ranges,
            ingredients,
        }
    }
}

fn overlapping(a: &Range, b: &Range) -> i64 {
    cmp::max(0, cmp::min(a.1, b.1) - cmp::max(a.0, b.0) + 1)
}
