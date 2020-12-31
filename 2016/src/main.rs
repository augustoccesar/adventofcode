mod day01;
mod day02;

mod task;

use std::collections::HashMap;
use std::env;

use day01::Day01;
use day02::Day02;
use task::Task;

fn days<'a>() -> HashMap<String, &'a dyn Task> {
    let mut days: HashMap<String, &'a dyn Task> = HashMap::new();
    let tasks: Vec<&'a dyn Task> = vec![&Day01 {}, &Day02 {}];
    for task in tasks {
        days.insert(task.day(), task);
    }
    return days;
}

fn run_task(task: &dyn Task) {
    task.part_one();
    task.part_two();
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let day_arg = &args[1];

    match days().get(day_arg) {
        Some(day) => run_task(*day),
        None => println!("Day not implemented."),
    }
}
