use std::convert::TryInto;

use aoc2023::{read_input, timed};

fn part_one() -> String {
    read_input("04")
        .lines()
        .map(|line| {
            let [_, numbers]: [&str; 2] =
                line.split(':').collect::<Vec<&str>>().try_into().unwrap();
            let [winning_numbers, owned_numbers]: [Vec<u64>; 2] = numbers
                .split('|')
                .map(|numbers| {
                    numbers
                        .split_whitespace()
                        .map(|number| number.parse::<u64>().unwrap())
                        .collect::<Vec<u64>>()
                })
                .collect::<Vec<Vec<u64>>>()
                .try_into()
                .unwrap();

            (winning_numbers, owned_numbers)
        })
        .map(|(winning_numbers, owned_numbers)| intersection(&winning_numbers, &owned_numbers))
        .map(|matching_numbers| match matching_numbers.len() {
            0 => 0,
            1 => 1,
            n => 1 << (n - 1),
        })
        .sum::<i32>()
        .to_string()
}

fn part_two() -> String {
    // (copies, winning numbers)
    let mut cards: Vec<(u64, usize)> = vec![];

    for line in read_input("04").lines() {
        let [_, numbers]: [&str; 2] = line.split(':').collect::<Vec<&str>>().try_into().unwrap();

        let [winning_numbers, owned_numbers]: [Vec<u64>; 2] = numbers
            .split('|')
            .map(|numbers| {
                numbers
                    .split_whitespace()
                    .map(|number| number.parse::<u64>().unwrap())
                    .collect::<Vec<u64>>()
            })
            .collect::<Vec<Vec<u64>>>()
            .try_into()
            .unwrap();

        let winning_numbers_count = intersection(&winning_numbers, &owned_numbers).len();

        cards.push((1, winning_numbers_count));
    }

    for i in 0..cards.len() {
        let current_card = cards[i];
        if current_card.1 == 0 {
            continue;
        }

        let start_copy_id = i + 1;
        let end_copy_id = i + current_card.1;
        (start_copy_id..=end_copy_id).for_each(|copy_id| {
            cards[copy_id].0 += current_card.0;
        });
    }

    cards.iter().map(|card| card.0).sum::<u64>().to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn intersection<T>(vec_1: &Vec<T>, vec_2: &Vec<T>) -> Vec<T>
where
    T: Eq + Copy,
{
    let mut vec_intersection = vec![];

    for item_vec_1 in vec_1 {
        for item_vec_2 in vec_2 {
            if item_vec_1 == item_vec_2 {
                vec_intersection.push(*item_vec_1);
            }
        }
    }

    vec_intersection
}
