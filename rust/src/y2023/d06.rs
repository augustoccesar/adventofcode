use crate::Day;

pub struct Day06 {}

impl Day for Day06 {
    fn year(&self) -> u16 {
        2023
    }

    fn day(&self) -> u8 {
        6
    }

    fn part_one(&self) -> String {
        let timesheet = parse_timesheet(&self.read_default_input());
        let times = &timesheet[0];
        let distances = &timesheet[1];
        let mut result: u64 = 1;

        for race_idx in 0..times.len() {
            let race_time = times[race_idx];
            let record_distance = distances[race_idx];

            let possible_hold_times = find_possible_hold_times(race_time, record_distance);

            result *= possible_hold_times;
        }

        result.to_string()
    }

    fn part_two(&self) -> String {
        let data = parse_timesheet_2(&self.read_default_input());
        let race_time = data[0];
        let record_distance = data[1];

        let possible_hold_times = find_possible_hold_times(race_time, record_distance);

        possible_hold_times.to_string()
    }
}

fn find_possible_hold_times(race_time: u64, record_distance: u64) -> u64 {
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

    possible_hold_times
}

fn parse_timesheet(input: &str) -> Vec<Vec<u64>> {
    input
        .lines()
        .map(|line| line.split(':').next_back().unwrap().trim())
        .map(|values| {
            values
                .split(' ')
                .filter(|item| item != &"")
                .map(|item| item.parse::<u64>().unwrap())
                .collect::<Vec<u64>>()
        })
        .collect::<Vec<Vec<u64>>>()
}

fn parse_timesheet_2(input: &str) -> Vec<u64> {
    input
        .lines()
        .map(|line| line.split(':').next_back().unwrap().trim())
        .map(|values| {
            values
                .split(' ')
                .filter(|item| item != &"")
                .collect::<Vec<&str>>()
                .join("")
                .parse::<u64>()
                .unwrap()
        })
        .collect::<Vec<u64>>()
}
