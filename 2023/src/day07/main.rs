use std::{collections::HashMap, convert::TryInto};

use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    let mut hands = read_input("07")
        .lines()
        .map(|line| {
            let tokens = line.split(' ').collect::<Vec<&str>>();
            let hand: [char; 5] = tokens[0].chars().collect::<Vec<char>>().try_into().unwrap();
            let strength = calculate_hand(&hand);

            (hand, strength, tokens[1].parse::<u32>().unwrap())
        })
        .collect::<Vec<([char; 5], u8, u32)>>();

    hands.sort_by(|left, right| {
        if left.1 != right.1 {
            return left.1.cmp(&right.1);
        }

        for i in 0..5 {
            let left_label = &left.0[i];
            let left_value = calculate_label(left_label);

            let right_label = &right.0[i];
            let right_value = calculate_label(right_label);

            let res = left_value.cmp(&right_value);

            if res.is_eq() {
                continue;
            }

            return res;
        }

        panic!("")
    });

    let mut result = 0;
    for (i, (_, _, bid)) in hands.iter().enumerate() {
        result += (i + 1) as u32 * bid;
    }

    result.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn calculate_label(label: &char) -> u8 {
    match label {
        'A' => 13,
        'K' => 12,
        'Q' => 11,
        'J' => 10,
        'T' => 9,
        '9' => 8,
        '8' => 7,
        '7' => 6,
        '6' => 5,
        '5' => 4,
        '4' => 3,
        '3' => 2,
        '2' => 1,
        _ => panic!("Invalid card label"),
    }
}

fn calculate_hand(hand: &[char; 5]) -> u8 {
    let mut agg: HashMap<char, u32> = HashMap::new();
    for card in hand {
        let count = agg.entry(*card).or_insert(0);
        *count += 1;
    }

    if agg.len() == 1 {
        // Five of a kind
        7
    } else if agg.len() == 2 {
        let max_count = *agg.values().max().unwrap();
        if max_count == 4 {
            // Four of a kind
            6
        } else {
            // Full house
            5
        }
    } else if agg.len() == 3 {
        let max_count = *agg.values().max().unwrap();
        if max_count == 3 {
            // Three of a kind
            4
        } else {
            // Two pairs
            3
        }
    } else if agg.len() == 4 {
        // One pair
        2
    } else {
        // High card
        1
    }
}
