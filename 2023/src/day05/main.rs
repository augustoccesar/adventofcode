use std::{collections::HashMap, convert::TryInto};

use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    let almanac = build_almanac(&read_input("05"));
    let mut closest_location = i64::MAX;
    for seed in &almanac.seeds {
        let location = almanac.traverse_categories(&Category::Seed, *seed);
        if location < closest_location {
            closest_location = location;
        }
    }

    closest_location.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}

#[derive(PartialEq, Eq, Hash, Clone)]
enum Category {
    Seed,
    Soil,
    Fertilizer,
    Water,
    Light,
    Temperature,
    Humidity,
    Location,
}

impl Category {
    fn next(&self) -> Option<Self> {
        match self {
            Category::Seed => Some(Self::Soil),
            Category::Soil => Some(Self::Fertilizer),
            Category::Fertilizer => Some(Self::Water),
            Category::Water => Some(Self::Light),
            Category::Light => Some(Self::Temperature),
            Category::Temperature => Some(Self::Humidity),
            Category::Humidity => Some(Self::Location),
            Category::Location => None,
        }
    }
}

type CategoryMap = HashMap<(Category, Category), Vec<(i64, i64, i64)>>;

struct Almanac {
    seeds: Vec<i64>,
    category_map: CategoryMap,
}

impl Almanac {
    fn traverse_categories(&self, current_category: &Category, input: i64) -> i64 {
        match current_category.next() {
            Some(next_category) => {
                let ranges = self
                    .category_map
                    .get(&(current_category.clone(), next_category.clone()))
                    .unwrap();

                let output = find_in_ranges(ranges, input);
                self.traverse_categories(&next_category, output)
            }
            None => input,
        }
    }
}

fn build_almanac(input: &str) -> Almanac {
    let mut maps: CategoryMap = CategoryMap::new();
    let mut seeds: Vec<i64> = vec![];

    for map in input.split("\n\n") {
        if map.starts_with("seeds:") {
            seeds = map
                .split(':')
                .last()
                .unwrap()
                .trim()
                .split(' ')
                .map(|number| number.parse::<i64>().unwrap())
                .collect::<Vec<i64>>();
            continue;
        }

        let map_lines = map.lines().collect::<Vec<&str>>();
        let map_key = match map_lines[0] {
            "seed-to-soil map:" => (Category::Seed, Category::Soil),
            "soil-to-fertilizer map:" => (Category::Soil, Category::Fertilizer),
            "fertilizer-to-water map:" => (Category::Fertilizer, Category::Water),
            "water-to-light map:" => (Category::Water, Category::Light),
            "light-to-temperature map:" => (Category::Light, Category::Temperature),
            "temperature-to-humidity map:" => (Category::Temperature, Category::Humidity),
            "humidity-to-location map:" => (Category::Humidity, Category::Location),
            _ => panic!("invalid map"),
        };

        let ranges = map_lines[1..]
            .iter()
            .map(|item| {
                item.trim()
                    .split(' ')
                    .map(|number| number.parse::<i64>().unwrap())
                    .collect::<Vec<i64>>()
            })
            .map(|range| (range[0], range[1], range[2]))
            .collect::<Vec<(i64, i64, i64)>>();

        maps.insert(map_key, ranges);
    }

    Almanac {
        seeds,
        category_map: maps,
    }
}

fn find_in_ranges(ranges: &[(i64, i64, i64)], lookup: i64) -> i64 {
    for (destination_start, source_start, length) in ranges {
        if lookup >= *source_start && lookup <= source_start + length {
            let distance = lookup - source_start;
            return destination_start + distance;
        }
    }

    lookup
}
