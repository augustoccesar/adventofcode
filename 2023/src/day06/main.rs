use aoc2023::{read_input, timed};

fn part_one() -> String {
    let timesheet = parse_timesheet(&read_input("06"));
    let times = &timesheet[0];
    let distances = &timesheet[1];
    let mut result = 1;

    for race_idx in 0..times.len() {
        let race_time = times[race_idx];
        let record_distance = distances[race_idx];

        let mut possible_hold_times = 0;
        let mut hold_time = race_time - 1;
        while hold_time > 0 {
            let time_to_race = race_time - hold_time;
            let velocity = hold_time;
            let distance_ran = velocity * time_to_race;
            if distance_ran > record_distance {
                possible_hold_times += 1;
            }

            hold_time -= 1;
        }

        result *= possible_hold_times;
    }

    result.to_string()
}

fn part_two() -> String {
    let data = parse_timesheet_2(&read_input("06"));
    let race_time = data[0];
    let record_distance = data[1];

    let mut possible_hold_times = 0;
    let mut hold_time = race_time - 1;
    while hold_time > 0 {
        let time_to_race = race_time - hold_time;
        let velocity = hold_time;
        let distance_ran = velocity * time_to_race;
        if distance_ran > record_distance {
            possible_hold_times += 1;
        }

        hold_time -= 1;
    }

    possible_hold_times.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn parse_timesheet(input: &str) -> Vec<Vec<i64>> {
    input
        .lines()
        .map(|line| line.split(':').last().unwrap().trim())
        .map(|values| {
            values
                .split(' ')
                .filter(|item| item != &"")
                .collect::<Vec<&str>>()
        })
        .map(|values| {
            values
                .iter()
                .map(|item| item.parse::<i64>().unwrap())
                .collect::<Vec<i64>>()
        })
        .collect::<Vec<Vec<i64>>>()
}

fn parse_timesheet_2(input: &str) -> Vec<i64> {
    input
        .lines()
        .map(|line| line.split(':').last().unwrap().trim())
        .map(|values| {
            values
                .split(' ')
                .filter(|item| item != &"")
                .collect::<Vec<&str>>()
                .join("")
                .parse::<i64>()
                .unwrap()
        })
        .collect::<Vec<i64>>()
}
