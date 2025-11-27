use crate::Day;

pub struct Day05 {}

impl Day for Day05 {
    fn year(&self) -> u16 {
        2023
    }

    fn day(&self) -> u8 {
        5
    }

    fn part_one(&self) -> String {
        let almanac = Almanac::from(self.read_default_input().as_str());
        let mut closest_location = i64::MAX;
        for seed in &almanac.seeds {
            let location = almanac.traverse_categories(Direction::Normal, *seed);
            if location < closest_location {
                closest_location = location;
            }
        }

        closest_location.to_string()
    }

    fn part_two(&self) -> String {
        let almanac = Almanac::from(self.read_default_input().as_str());
        let mut closest_location = 0;
        let seed_ranges: Vec<(i64, i64)> = almanac.seeds_as_ranges();

        'outer: loop {
            let seed = almanac.traverse_categories(Direction::Reverse, closest_location);
            for (range_start, range_end) in &seed_ranges {
                if seed >= *range_start && seed < *range_end {
                    break 'outer;
                }
            }

            closest_location += 1;
        }

        closest_location.to_string()
    }
}

enum Direction {
    Normal,
    Reverse,
}

struct Almanac {
    seeds: Vec<i64>,
    steps: Vec<Vec<(i64, i64, i64)>>,
}

impl Almanac {
    fn seeds_as_ranges(&self) -> Vec<(i64, i64)> {
        self.seeds
            .chunks(2)
            .map(|chunk| (chunk[0], chunk[0] + chunk[1]))
            .collect()
    }

    fn traverse_categories(&self, direction: Direction, input: i64) -> i64 {
        match direction {
            Direction::Normal => self.traverse_categories_recursion(direction, -1, input),
            Direction::Reverse => self.traverse_categories_recursion(direction, 7, input),
        }
    }

    fn traverse_categories_recursion(
        &self,
        direction: Direction,
        current_step: i8,
        input: i64,
    ) -> i64 {
        let next_step_idx = match direction {
            Direction::Normal => current_step + 1,
            Direction::Reverse => current_step - 1,
        };

        if !(0..=6).contains(&next_step_idx) {
            return input;
        }

        let next_step_idx = next_step_idx as usize;
        let next_step = &self.steps[next_step_idx];

        let mut output = input;
        for (destination_start, source_start, length) in next_step {
            let (left, right) = match direction {
                Direction::Normal => (source_start, destination_start),
                Direction::Reverse => (destination_start, source_start),
            };

            if input >= *left && input < left + length {
                let distance = input - left;
                output = right + distance;
            }
        }

        self.traverse_categories_recursion(direction, next_step_idx as i8, output)
    }
}

impl From<&str> for Almanac {
    fn from(value: &str) -> Self {
        let mut seeds: Vec<i64> = vec![];
        let mut steps: Vec<Vec<(i64, i64, i64)>> = Vec::new();
        for _ in 0..7 {
            steps.push(Vec::new());
        }

        for map in value.split("\n\n") {
            if map.starts_with("seeds:") {
                seeds = map
                    .split(':')
                    .next_back()
                    .unwrap()
                    .trim()
                    .split(' ')
                    .map(|number| number.parse::<i64>().unwrap())
                    .collect::<Vec<i64>>();
                continue;
            }

            let map_lines = map.lines().collect::<Vec<&str>>();
            let step_idx = match map_lines[0] {
                "seed-to-soil map:" => 0,
                "soil-to-fertilizer map:" => 1,
                "fertilizer-to-water map:" => 2,
                "water-to-light map:" => 3,
                "light-to-temperature map:" => 4,
                "temperature-to-humidity map:" => 5,
                "humidity-to-location map:" => 6,
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

            steps[step_idx] = ranges;
        }

        Self { seeds, steps }
    }
}
