use aoc2023::{read_input, timed};

fn part_one() -> String {
    let histories = parse_input(&read_input("09"));

    let mut res = 0;
    for history in &histories {
        let mut diff_sequences: Vec<Vec<i32>> = Vec::new();
        diff_sequences.push(history.clone());

        generate_diff_sequence(history, &mut diff_sequences);
        calculate_diff_sequences_level(&mut diff_sequences, |x, y| x + y);

        res += diff_sequences[0].last().unwrap();
    }

    res.to_string()
}

fn part_two() -> String {
    let histories = parse_input(&read_input("09"));

    let mut res = 0;
    for history in &histories {
        let mut diff_sequences: Vec<Vec<i32>> = Vec::new();
        diff_sequences.push(history.clone());

        generate_diff_sequence(history, &mut diff_sequences);
        diff_sequences.iter_mut().for_each(|seq| seq.reverse());

        calculate_diff_sequences_level(&mut diff_sequences, |x, y| x - y);

        res += diff_sequences[0].last().unwrap();
    }

    res.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    input
        .lines()
        .map(|line| line.split(' ').collect::<Vec<&str>>())
        .map(|history| {
            history
                .iter()
                .map(|item| item.parse::<i32>().unwrap())
                .collect::<Vec<i32>>()
        })
        .collect::<Vec<Vec<i32>>>()
}

fn generate_diff_sequence(history: &Vec<i32>, diff_sequences: &mut Vec<Vec<i32>>) {
    let mut diff_sequence: Vec<i32> = Vec::with_capacity(history.len() - 1);

    for i in 0..history.len() - 1 {
        let diff = history[i + 1] - history[i];
        diff_sequence.push(diff);
    }

    diff_sequences.push(diff_sequence.clone());

    if diff_sequence.iter().all(|&x| x == 0) {
        return;
    }

    generate_diff_sequence(&diff_sequence, diff_sequences);
}

fn calculate_diff_sequences_level<F>(diff_sequences: &mut Vec<Vec<i32>>, function: F)
where
    F: Fn(i32, i32) -> i32,
{
    let mut i = diff_sequences.len() as i32 - 2;
    while i >= 0 {
        let usize_i = i as usize;
        let level_value = function(
            *diff_sequences[usize_i].last().unwrap(),
            *diff_sequences[usize_i + 1].last().unwrap(),
        );
        diff_sequences[usize_i].push(level_value);
        i -= 1;
    }
}
