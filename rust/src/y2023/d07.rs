use std::collections::HashMap;

use crate::Day;

pub struct Day07 {}

impl Day for Day07 {
    fn year(&self) -> u16 {
        2023
    }

    fn day(&self) -> u8 {
        7
    }

    fn part_one(&self) -> String {
        let wildcard = false;
        let mut hands = parse_input(&self.read_default_input(), wildcard);

        sort_hands(&mut hands, wildcard);
        total_winnings(&hands).to_string()
    }

    fn part_two(&self) -> String {
        let wildcard = true;
        let mut hands = parse_input(&self.read_default_input(), wildcard);

        sort_hands(&mut hands, wildcard);
        total_winnings(&hands).to_string()
    }
}

type Hand = ([char; 5], HandType, u32);

fn parse_input(input: &str, wildcard: bool) -> Vec<Hand> {
    input
        .lines()
        .map(|line| {
            let tokens = line.split(' ').collect::<Vec<&str>>();
            let hand: [char; 5] = tokens[0].chars().collect::<Vec<char>>().try_into().unwrap();
            let strength = calculate_hand(&hand, wildcard);

            (hand, strength, tokens[1].parse::<u32>().unwrap())
        })
        .collect::<Vec<Hand>>()
}

fn sort_hands(hands: &mut [Hand], wildcard: bool) {
    hands.sort_by(|left, right| {
        if left.1 != right.1 {
            return left.1.partial_cmp(&right.1).unwrap();
        }

        for i in 0..5 {
            let left_label = left.0[i];
            let left_value = calculate_label(left_label, wildcard);

            let right_label = right.0[i];
            let right_value = calculate_label(right_label, wildcard);

            let res = left_value.cmp(&right_value);

            if res.is_eq() {
                continue;
            }

            return res;
        }

        unreachable!()
    });
}

fn total_winnings(hands: &[Hand]) -> u32 {
    let mut result = 0;
    for (i, (_, _, bid)) in hands.iter().enumerate() {
        result += (i + 1) as u32 * bid;
    }

    result
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
        _ => unreachable!(),
    }
}

fn calculate_hand(hand: &[char; 5], wildcard: bool) -> HandType {
    let mut agg: HashMap<char, u32> = HashMap::new();
    for card in hand {
        let count = agg.entry(*card).or_insert(0);
        *count += 1;
    }

    let default_strength = if agg.len() == 1 {
        HandType::FiveOfAKind
    } else if agg.len() == 2 {
        let max_count = *agg.values().max().unwrap();
        if max_count == 4 {
            HandType::FourOfAKind
        } else {
            HandType::FullHouse
        }
    } else if agg.len() == 3 {
        let max_count = *agg.values().max().unwrap();
        if max_count == 3 {
            HandType::ThreeOfAKind
        } else {
            HandType::TwoPair
        }
    } else if agg.len() == 4 {
        HandType::OnePair
    } else {
        HandType::HighCard
    };

    if !wildcard || !hand.contains(&'J') {
        return default_strength;
    }

    match default_strength {
        HandType::FiveOfAKind => HandType::FiveOfAKind,
        HandType::FourOfAKind => HandType::FiveOfAKind,
        HandType::FullHouse => HandType::FiveOfAKind,
        HandType::ThreeOfAKind => HandType::FourOfAKind,
        HandType::TwoPair => {
            let j_count = hand.iter().filter(|label| **label == 'J').count();
            if j_count == 1 {
                HandType::FullHouse
            } else if j_count == 2 {
                HandType::FourOfAKind
            } else {
                unreachable!()
            }
        }
        HandType::OnePair => HandType::ThreeOfAKind,
        HandType::HighCard => HandType::OnePair,
    }
}

#[derive(PartialEq, Eq)]
enum HandType {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

impl HandType {
    fn value(&self) -> u8 {
        match self {
            HandType::FiveOfAKind => 7,
            HandType::FourOfAKind => 6,
            HandType::FullHouse => 5,
            HandType::ThreeOfAKind => 4,
            HandType::TwoPair => 3,
            HandType::OnePair => 2,
            HandType::HighCard => 1,
        }
    }
}

impl PartialOrd for HandType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.value().cmp(&other.value()))
    }
}
