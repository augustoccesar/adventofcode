use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    let mut platform = read_input("14")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    for y in 0..platform.len() {
        for x in 0..platform[0].len() {
            if platform[y][x] == 'O' {
                let mut new_y = y;
                while let Some(sub_y) = new_y.checked_sub(1) {
                    if new_y == 0 {
                        break;
                    }

                    match platform[sub_y][x] {
                        '.' => new_y = sub_y,
                        'O' | '#' => break,
                        _ => unreachable!(),
                    }
                }

                if new_y != y {
                    platform[y][x] = '.';
                    platform[new_y][x] = 'O';
                }
            }
        }
    }

    let mut total_load = 0;
    for y in 0..platform.len() {
        total_load +=
            platform[y].iter().filter(|item| **item == 'O').count() * (platform.len() - y);
    }

    total_load.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}
