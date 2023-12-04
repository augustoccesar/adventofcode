use std::convert::TryInto;

use aoc2023::{read_input, timed};

fn part_one() -> String {
    read_input("04")
        .lines()
        .map(count_card_winning_numbers)
        .map(|matching_numbers_count| match matching_numbers_count {
            0 => 0,
            1 => 1,
            n => 1 << (n - 1),
        })
        .sum::<i32>()
        .to_string()
}

fn part_two() -> String {
    // (copies, winning_numbers_count)
    let mut cards: Vec<(u64, usize)> = vec![];

    for (idx, line) in read_input("04").lines().enumerate() {
        let winning_numbers_count = count_card_winning_numbers(line);

        match cards.get_mut(idx) {
            Some(card) => {
                card.0 += 1;
                card.1 = winning_numbers_count;
            }
            None => cards.push((1, winning_numbers_count)),
        }

        for copy_idx in (idx + 1)..=(idx + winning_numbers_count) {
            let current_card = cards[idx];
            match cards.get_mut(copy_idx) {
                Some(card) => card.0 += current_card.0,
                None => cards.push((current_card.0, 0)),
            }
        }
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

fn count_card_winning_numbers(card_str: &str) -> usize {
    let [_, numbers]: [&str; 2] = card_str
        .split(':')
        .collect::<Vec<&str>>()
        .try_into()
        .unwrap();

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

    intersection(&winning_numbers, &owned_numbers).len()
}
