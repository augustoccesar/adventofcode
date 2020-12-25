use std::fs;

use crate::task::Task;

// const directions: [&str; 4] = ["n", "e", "s", "w"];
const DIRECTIONS: [(i8, i8); 4] = [
    // x, y
    (0, 1),  // N - 0
    (1, 0),  // E - 1
    (0, -1), // S - 2
    (-1, 0), // W - 3
];

#[derive(Debug)]
struct Point {
    x: i64,
    y: i64,
    facing: usize,
}

impl<'a> Point {
    fn exec_command(&mut self, command: &str) {
        let direction = &command[0..1];
        let amount = command[1..].parse::<i64>().unwrap();

        let mut new_facing: usize = self.facing;
        if direction == "R" {
            new_facing += 1;
            if new_facing == 4 {
                new_facing = 0
            }
        } else if direction == "L" {
            match new_facing.checked_sub(1) {
                Some(val) => new_facing = val,
                None => new_facing = 3,
            }
        }

        self.facing = new_facing;
        let direction_modifier: (i8, i8) = DIRECTIONS[new_facing];
        self.x = self.x + (i64::from(direction_modifier.0) * amount);
        self.y = self.y + (i64::from(direction_modifier.1) * amount);
    }

    fn taxicab_distance(self, x: i64, y: i64) -> i64 {
        return (self.x - x).abs() + (self.y - y).abs();
    }
}

pub struct Day01 {}

impl Task for Day01 {
    fn part_one(&self) {
        let contents = fs::read_to_string("inputs/day01_input.txt").unwrap();
        let commands: Vec<&str> = contents.split(", ").collect();
        let mut point = Point {
            x: 0,
            y: 0,
            facing: 0,
        };

        for command in commands {
            point.exec_command(command);
        }
        println!("Part One: {:?}", point.taxicab_distance(0, 0));
    }
    fn part_two(&self) {}
}
