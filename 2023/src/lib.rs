use std::{fs, time::Instant};

pub fn timed<F>(function: F)
where
    F: FnOnce() -> String,
{
    let start = Instant::now();
    let result = function();
    let duration = start.elapsed();
    println!("{}", result);
    println!("({:?})\n", duration);
}

pub fn read_input(day: &str) -> String {
    fs::read_to_string(format!("inputs/day{}_input.txt", day)).unwrap()
}

pub fn read_named_input(day: &str, name: &str) -> String {
    fs::read_to_string(format!("inputs/day{}_{}.txt", day, name)).unwrap()
}
