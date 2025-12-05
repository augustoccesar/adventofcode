use std::ops::RangeInclusive;

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
        "-".to_owned()
    }
}
