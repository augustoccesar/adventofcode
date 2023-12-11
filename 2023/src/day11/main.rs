use std::collections::HashSet;

use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    let mut map = read_input("11")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let mut rows_has_galaxy: Vec<bool> = vec![false; map.len()];
    let mut cols_has_galaxy: Vec<bool> = vec![false; map[0].len()];

    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if map[y][x] == '#' {
                rows_has_galaxy[y] = true;
                cols_has_galaxy[x] = true;
            }
        }
    }

    let mut mod_x = 0;
    let mut mod_y = 0;
    for (i, has_galaxy) in rows_has_galaxy.iter().enumerate() {
        if *has_galaxy {
            continue;
        }

        map.insert(i + mod_y, vec!['.'; map[0].len()]);
        mod_y += 1;
    }

    for (i, has_galaxy) in cols_has_galaxy.iter().enumerate() {
        if *has_galaxy {
            continue;
        }

        map.iter_mut().for_each(|row| {
            row.insert(i + mod_x, '.');
        });

        mod_x += 1;
    }

    let mut galaxies: Vec<(usize, usize)> = Vec::new();
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if map[y][x] == '#' {
                galaxies.push((x, y));
            }
        }
    }

    let mut galaxies_pairs: HashSet<(usize, usize)> = HashSet::new();
    for i in 0..galaxies.len() {
        for j in i..galaxies.len() {
            if j == i {
                continue;
            }

            let left = if i < j { i } else { j };
            let right = if i > j { i } else { j };

            galaxies_pairs.insert((left, right));
        }
    }

    let mut res = 0;
    for (idx_left, idx_right) in galaxies_pairs {
        let galaxy_1 = galaxies[idx_left];
        let galaxy_2 = galaxies[idx_right];

        res += galaxy_1.0.abs_diff(galaxy_2.0) + galaxy_1.1.abs_diff(galaxy_2.1);
    }

    res.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}
