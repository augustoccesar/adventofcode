use std::convert::TryInto;

use aoc2023::{read_input, timed};

fn part_one() -> String {
    let (config_red, config_green, config_blue) = (12, 13, 14);

    let result = read_input("02")
        .lines()
        .map(Game::from)
        .map(|game| {
            for round in game.rounds {
                if round.red > config_red {
                    return 0;
                }

                if round.green > config_green {
                    return 0;
                }

                if round.blue > config_blue {
                    return 0;
                }
            }

            game.id
        })
        .sum::<u64>();

    result.to_string()
}

fn part_two() -> String {
    let result = read_input("02")
        .lines()
        .map(Game::from)
        .map(|game| {
            let [mut max_red, mut max_green, mut max_blue]: [_; 3] = [0, 0, 0];

            for round in game.rounds {
                if round.red > max_red {
                    max_red = round.red;
                }

                if round.green > max_green {
                    max_green = round.green;
                }

                if round.blue > max_blue {
                    max_blue = round.blue;
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

struct Game {
    id: u64,
    rounds: Vec<Round>,
}

impl From<&str> for Game {
    fn from(value: &str) -> Self {
        let parts = value.split(':').collect::<Vec<&str>>();
        let game_id = parts
            .first()
            .unwrap()
            .split(' ')
            .last()
            .map(|item| item.parse::<u64>().unwrap())
            .unwrap();

        let rounds = parts
            .get(1)
            .unwrap()
            .split(';')
            .map(Round::from)
            .collect::<Vec<Round>>();

        Self {
            id: game_id,
            rounds,
        }
    }
}

struct Round {
    red: u64,
    green: u64,
    blue: u64,
}

impl From<&str> for Round {
    fn from(value: &str) -> Self {
        let [mut red, mut green, mut blue]: [_; 3] = [0, 0, 0];
        let picks = value.split(',').map(|pick| pick.trim());
        for pick in picks {
            let [quantity, color]: [_; 2] =
                pick.split(' ').collect::<Vec<&str>>().try_into().unwrap();
            let quantity = quantity.parse::<u64>().unwrap();

            match color {
                "red" => red = quantity,
                "green" => green = quantity,
                "blue" => blue = quantity,
                _ => panic!("Invalid color"),
            }
        }

        Self { red, green, blue }
    }
}
