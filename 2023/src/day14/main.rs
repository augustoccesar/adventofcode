use std::collections::{hash_map::Entry, HashMap};

use aoc2023::{read_input, timed, Direction};

fn part_one() -> String {
    let mut platform = read_input("14")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    apply_gravity(&mut platform, &Direction::North);

    calculate_load(&platform).to_string()
}

fn part_two() -> String {
    let mut platform = read_input("14")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let mut cache: HashMap<String, usize> = HashMap::new();
    let mut cycle_load: HashMap<usize, usize> = HashMap::new();

    for i in 1..=1_000_000_000 {
        let serialized_platform = serialize(&platform);

        match cache.entry(serialized_platform.clone()) {
            Entry::Occupied(last_occurrence) => {
                let last_occurrence = *last_occurrence.get();
                let cycles_from_repeating = 1_000_000_000 - last_occurrence;

                let repeating_size = i - last_occurrence;
                let distance_to_target = cycles_from_repeating % repeating_size;

                let target = last_occurrence + distance_to_target;
                return cycle_load.get(&target).unwrap().to_string();
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(i);
            }
        }

        cycle_platform(&mut platform);

        cycle_load.insert(i, calculate_load(&platform));
    }

    unreachable!()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn cycle_platform(platform: &mut Vec<Vec<char>>) {
    for direction in Direction::iter() {
        apply_gravity(platform, &direction);
    }
}

fn apply_gravity(platform: &mut Vec<Vec<char>>, direction: &Direction) {
    let (x_mod, y_mod) = direction.modifier();

    let x_iter = match direction {
        Direction::East => (0..platform[0].len()).rev().collect::<Vec<_>>(),
        _ => (0..platform[0].len()).collect::<Vec<_>>(),
    };

    let y_iter = match direction {
        Direction::South => (0..platform.len()).rev().collect::<Vec<_>>(),
        _ => (0..platform.len()).collect::<Vec<_>>(),
    };

    for y in y_iter {
        for x in x_iter.clone() {
            if platform[y][x] == 'O' {
                let mut new_y = y;
                let mut new_x = x;

                let mut next_y = y as i32 + y_mod;
                let mut next_x = x as i32 + x_mod;
                while (next_y < platform.len() as i32 && next_y >= 0)
                    && (next_x < platform[0].len() as i32 && next_x >= 0)
                {
                    match platform[next_y as usize][next_x as usize] {
                        '.' => {
                            new_x = next_x as usize;
                            new_y = next_y as usize;
                        }
                        'O' | '#' => break,
                        _ => unreachable!(),
                    }

                    next_y += y_mod;
                    next_x += x_mod;
                }

                if new_x != x || new_y != y {
                    platform[y][x] = '.';
                    platform[new_y][new_x] = 'O';
                }
            }
        }
    }
}

fn calculate_load(platform: &[Vec<char>]) -> usize {
    let mut total_load = 0;
    for y in 0..platform.len() {
        total_load +=
            platform[y].iter().filter(|item| **item == 'O').count() * (platform.len() - y);
    }

    total_load
}

fn serialize(platform: &[Vec<char>]) -> String {
    platform.iter().flatten().collect::<String>()
}
