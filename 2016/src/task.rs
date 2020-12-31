use std::fs;

pub trait Task {
    fn day(&self) -> String;
    fn part_one(&self);
    fn part_two(&self);
    fn read_input(&self) -> String {
        return fs::read_to_string(format!("inputs/day{}_input.txt", self.day())).unwrap();
    }
}
