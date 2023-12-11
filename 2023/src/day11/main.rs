use std::cmp;

use aoc2023::{read_input, timed};

fn part_one() -> String {
    let (galaxies, rows_has_galaxy, cols_has_galaxy) = parse_input(&read_input("11"));

    distances_sum(&galaxies, 2, &rows_has_galaxy, &cols_has_galaxy).to_string()
}

fn part_two() -> String {
    let (galaxies, rows_has_galaxy, cols_has_galaxy) = parse_input(&read_input("11"));

    distances_sum(&galaxies, 1_000_000, &rows_has_galaxy, &cols_has_galaxy).to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn parse_input(input: &str) -> (Vec<(usize, usize)>, Vec<bool>, Vec<bool>) {
    let map = input
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let mut galaxies: Vec<(usize, usize)> = Vec::new();
    let mut rows_has_galaxy: Vec<bool> = vec![false; map.len()];
    let mut cols_has_galaxy: Vec<bool> = vec![false; map[0].len()];

    for (y, row) in map.iter().enumerate() {
        for (x, item) in row.iter().enumerate() {
            if item == &'#' {
                galaxies.push((x, y));
                rows_has_galaxy[y] = true;
                cols_has_galaxy[x] = true;
            }
        }
    }

    (galaxies, rows_has_galaxy, cols_has_galaxy)
}

fn distances_sum(
    galaxies: &[(usize, usize)],
    expansion_rate: usize,
    rows_has_galaxy: &[bool],
    cols_has_galaxy: &[bool],
) -> usize {
    let mut sum = 0;
    for i in 0..galaxies.len() {
        let galaxy_1 = galaxies[i];
        for galaxy_2 in galaxies.iter().skip(i) {
            if &galaxy_1 == galaxy_2 {
                continue;
            }

            let min_x = cmp::min(galaxy_1.0, galaxy_2.0);
            let max_x = cmp::max(galaxy_1.0, galaxy_2.0);
            let min_y = cmp::min(galaxy_1.1, galaxy_2.1);
            let max_y = cmp::max(galaxy_1.1, galaxy_2.1);

            let mut expansion_x = 0;
            let mut expansion_y = 0;

            for (row_ids, has_galaxy) in rows_has_galaxy.iter().enumerate() {
                if *has_galaxy {
                    continue;
                }

                if min_y < row_ids && max_y > row_ids {
                    expansion_y += expansion_rate - 1;
                }
            }

            for (col_idx, has_galaxy) in cols_has_galaxy.iter().enumerate() {
                if *has_galaxy {
                    continue;
                }

                if min_x < col_idx && max_x > col_idx {
                    expansion_x += expansion_rate - 1;
                }
            }

            sum += (max_x - min_x) + (max_y - min_y) + expansion_x + expansion_y;
        }
    }

    sum
}
