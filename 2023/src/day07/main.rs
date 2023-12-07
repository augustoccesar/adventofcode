use std::{collections::HashMap, convert::TryInto};

use aoc2023::{read_input, timed};

fn part_one() -> String {
    let mut hands = read_input("07")
        .lines()
        .map(|line| {
            let tokens = line.split(' ').collect::<Vec<&str>>();
            let hand: [char; 5] = tokens[0].chars().collect::<Vec<char>>().try_into().unwrap();
            let strength = calculate_hand(&hand, false);

            (hand, strength, tokens[1].parse::<u32>().unwrap())
        })
        .collect::<Vec<([char; 5], u8, u32)>>();

    hands.sort_by(|left, right| {
        if left.1 != right.1 {
            return left.1.cmp(&right.1);
        }

        for i in 0..5 {
            let left_label = left.0[i];
            let left_value = calculate_label(left_label, false);

            let right_label = right.0[i];
            let right_value = calculate_label(right_label, false);

            let res = left_value.cmp(&right_value);

            if res.is_eq() {
                continue;
            }

            return res;
        }

        unreachable!()
    });

    let mut result = 0;
    for (i, (_, _, bid)) in hands.iter().enumerate() {
        result += (i + 1) as u32 * bid;
    }

    result.to_string()
}

fn part_two() -> String {
    let mut hands = read_input("07")
        .lines()
        .map(|line| {
            let tokens = line.split(' ').collect::<Vec<&str>>();
            let hand: [char; 5] = tokens[0].chars().collect::<Vec<char>>().try_into().unwrap();
            let strength = calculate_hand(&hand, true);

            (hand, strength, tokens[1].parse::<u32>().unwrap())
        })
        .collect::<Vec<([char; 5], u8, u32)>>();

    hands.sort_by(|left, right| {
        if left.1 != right.1 {
            return left.1.cmp(&right.1);
        }

        for i in 0..5 {
            let left_label = left.0[i];
            let left_value = calculate_label(left_label, true);

            let right_label = right.0[i];
            let right_value = calculate_label(right_label, true);

            let res = left_value.cmp(&right_value);

            if res.is_eq() {
                continue;
            }

            return res;
        }

        unreachable!()
    });

    let mut result = 0;
    for (i, (_, _, bid)) in hands.iter().enumerate() {
        result += (i + 1) as u32 * bid;
    }

    result.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn calculate_label(label: char, wildcard: bool) -> u8 {
    match label {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' => {
            if wildcard {
                1
            } else {
                11
            }
        }
        'T' => 10,
        '9' => 9,
        '8' => 8,
        '7' => 7,
        '6' => 6,
        '5' => 5,
        '4' => 4,
        '3' => 3,
        '2' => 2,
        _ => panic!("Invalid card label"),
    }
}

fn calculate_hand(hand: &[char; 5], wildcard: bool) -> u8 {
    let mut agg: HashMap<char, u32> = HashMap::new();
    for card in hand {
        let count = agg.entry(*card).or_insert(0);
        *count += 1;
    }

    let default_strength = if agg.len() == 1 {
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
    };

    if !wildcard || !hand.contains(&'J') {
        return default_strength;
    }

    let j_count = hand.iter().filter(|label| **label == 'J').count();
    match default_strength {
        7 => 7,
        6 => 7,
        5 => 7,
        4 => {
            if j_count == 1 || j_count == 3 {
                6
            } else {
                unreachable!()
            }
        }
        3 => {
            if j_count == 1 {
                5
            } else if j_count == 2 {
                6
            } else {
                unreachable!()
            }
        }
        2 => 4,
        1 => 2,
        _ => unreachable!(),
    }
}

