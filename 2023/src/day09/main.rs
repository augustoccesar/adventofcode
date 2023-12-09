use aoc2023::{read_input, read_named_input, timed};

fn part_one() -> String {
    let histories = read_input("09")
        .lines()
        .map(|line| line.split(' ').collect::<Vec<&str>>())
        .map(|history| {
            history
                .iter()
                .map(|item| item.parse::<i32>().unwrap())
                .collect::<Vec<i32>>()
        })
        .collect::<Vec<Vec<i32>>>();

    let mut res = 0;

    for history in &histories {
        let mut diff_sequences: Vec<Vec<i32>> = Vec::new();
        diff_sequences.push(history.clone());

        generate_diff_sequence(history, &mut diff_sequences);

        let mut i = 1;
        while i < diff_sequences.len() {
            let level_value =
                diff_sequences[i].last().unwrap() + diff_sequences[i - 1].last().unwrap();
            diff_sequences[i].push(level_value);
            i += 1;
        }

        let history_value = diff_sequences.last().unwrap().last().unwrap();
        res += history_value;
    }

    res.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
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
