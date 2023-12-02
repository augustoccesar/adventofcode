use std::convert::TryInto;

use aoc2023::{read_input, timed};

fn part_one() -> String {
    let (config_red, config_green, config_blue) = (12, 13, 14);

    let result = read_input("02")
        .lines()
        .map(|item| {
            let parts = item.split(':').collect::<Vec<&str>>();
            let game_id = parts
                .first()
                .unwrap()
                .split(' ')
                .last()
                .map(|item| item.parse::<u64>().unwrap())
                .unwrap();

            let rounds = parts.get(1).unwrap().split(';').collect::<Vec<&str>>();
            for round in rounds {
                let picks = round.split(',').map(|pick| pick.trim());
                for pick in picks {
                    let [quantity, color]: [_; 2] =
                        pick.split(' ').collect::<Vec<&str>>().try_into().unwrap();
                    let quantity = quantity.parse::<u8>().unwrap();

                    if color == "red" && quantity > config_red {
                        return 0;
                    }

                    if color == "green" && quantity > config_green {
                        return 0;
                    }

                    if color == "blue" && quantity > config_blue {
                        return 0;
                    }
                }
            }

            game_id
        })
        .sum::<u64>();

    result.to_string()
}

fn part_two() -> String {
    let result = read_input("02")
        .lines()
        .map(|item| {
            let parts = item.split(':').collect::<Vec<&str>>();

            let [mut max_red, mut max_green, mut max_blue]: [_; 3] = [0, 0, 0];
            let rounds = parts.get(1).unwrap().split(';').collect::<Vec<&str>>();
            for round in rounds {
                let picks = round.split(',').map(|pick| pick.trim());
                for pick in picks {
                    let [quantity, color]: [_; 2] =
                        pick.split(' ').collect::<Vec<&str>>().try_into().unwrap();
                    let quantity = quantity.parse::<u64>().unwrap();

                    if color == "red" && quantity > max_red {
                        max_red = quantity;
                    }

                    if color == "green" && quantity > max_green {
                        max_green = quantity;
                    }

                    if color == "blue" && quantity > max_blue {
                        max_blue = quantity;
                    }
                }
            }

            max_red * max_green * max_blue
        })
        .sum::<u64>();

    result.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}
