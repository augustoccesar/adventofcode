use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use aoc2023::{read_input, timed};

fn part_one() -> String {
    let almanac = build_almanac(&read_input("05"));
    let mut closest_location = i64::MAX;
    for seed in &almanac.seeds {
        let location = almanac.traverse_categories(Direction::Normal, &Category::Seed, *seed);
        if location < closest_location {
            closest_location = location;
        }
    }

    closest_location.to_string()
}

fn part_two() -> String {
    let almanac = build_almanac(&read_input("05"));
    let mut closest_location = 0;
    let seed_ranges: Vec<(i64, i64)> = almanac.seeds_as_ranges();

    'outer: loop {
        let seed =
            almanac.traverse_categories(Direction::Reverse, &Category::Location, closest_location);
        for (range_start, range_end) in &seed_ranges {
            if seed >= *range_start && seed < *range_end {
                break 'outer;
            }
        }

        closest_location += 1;
    }

    closest_location.to_string()
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

    fn previous(&self) -> Option<Self> {
        match self {
            Category::Seed => None,
            Category::Soil => Some(Self::Seed),
            Category::Fertilizer => Some(Self::Soil),
            Category::Water => Some(Self::Fertilizer),
            Category::Light => Some(Self::Water),
            Category::Temperature => Some(Self::Light),
            Category::Humidity => Some(Self::Temperature),
            Category::Location => Some(Self::Humidity),
        }
    }
}

type InnerCategoryMap = HashMap<(Category, Category), Vec<(i64, i64, i64)>>;
struct CategoryMap(InnerCategoryMap);

impl CategoryMap {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn find_by_destination(&self, key: &(Category, Category), lookup: i64) -> i64 {
        let ranges = self.0.get(key).unwrap();

        for (destination_start, source_start, length) in ranges {
            if lookup >= *destination_start && lookup < destination_start + length {
                let distance = lookup - destination_start;
                return source_start + distance;
            }
        }

        lookup
    }

    fn find_by_source(&self, key: &(Category, Category), lookup: i64) -> i64 {
        let ranges = self.0.get(key).unwrap();

        for (destination_start, source_start, length) in ranges {
            if lookup >= *source_start && lookup < source_start + length {
                let distance = lookup - source_start;
                return destination_start + distance;
            }
        }

        lookup
    }
}

impl Deref for CategoryMap {
    type Target = InnerCategoryMap;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CategoryMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

struct Almanac {
    seeds: Vec<i64>,
    category_map: CategoryMap,
}

impl Almanac {
    fn seeds_as_ranges(&self) -> Vec<(i64, i64)> {
        self.seeds
            .chunks(2)
            .map(|chunk| (chunk[0], chunk[0] + chunk[1]))
            .collect()
    }

    fn traverse_categories(
        &self,
        direction: Direction,
        current_category: &Category,
        input: i64,
    ) -> i64 {
        match direction {
            Direction::Normal => match current_category.next() {
                Some(next_category) => {
                    let key = (current_category.clone(), next_category.clone());
                    let output = self.category_map.find_by_source(&key, input);
                    self.traverse_categories(direction, &next_category, output)
                }
                None => input,
            },
            Direction::Reverse => match current_category.previous() {
                Some(previous_category) => {
                    let key = (previous_category.clone(), current_category.clone());
                    let output = self.category_map.find_by_destination(&key, input);
                    self.traverse_categories(direction, &previous_category, output)
                }
                None => input,
            },
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

enum Direction {
    Normal,
    Reverse,
}
