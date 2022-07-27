mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
//SETUP:target_mod

mod task;

use std::collections::HashMap;
use std::env;
use std::time::Instant;

use day01::Day01;
use day02::Day02;
use day03::Day03;
use day04::Day04;
use day05::Day05;
use day06::Day06;
use day07::Day07;
use day08::Day08;
use day09::Day09;
use day10::Day10;
//SETUP:target_use
use task::Task;

// NOTE: Add format skip here so the SETUP label stay on the start of the line
#[rustfmt::skip]
fn days<'a>() -> HashMap<String, &'a dyn Task> {
    let mut days: HashMap<String, &'a dyn Task> = HashMap::new();
    let tasks: Vec<&'a dyn Task> = vec![
        &Day01 {},
        &Day02 {},
        &Day03 {},
        &Day04 {},
        &Day05 {},
        &Day06 {},
        &Day07 {},
        &Day08 {},
        &Day09 {},
        &Day10 {},
//SETUP:target_tasks
    ];
    for task in tasks {
        days.insert(task.day(), task);
    }
    return days;
}

fn run_task(task: &dyn Task) {
    let start = Instant::now();
    task.part_one();
    let duration = start.elapsed();
    println!("({:?})\n", duration);

    let start = Instant::now();
    task.part_two();
    let duration = start.elapsed();
    println!("({:?})\n", duration);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let day_arg = &args[1];

    match days().get(day_arg) {
        Some(day) => run_task(*day),
        None => println!("Day not implemented."),
    }
}
