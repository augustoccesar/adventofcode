use crate::task::Task;

use std::collections::HashMap;
use std::str::FromStr;

use lazy_static::lazy_static;
use regex::Regex;

pub struct Day10 {}

impl Task for Day10 {
    fn day(&self) -> String {
        return String::from("10");
    }

    fn part_one(&self) {
        let (exec_bot, _) = execute(self.read_input(), [61, 17]);
        println!("Part One: {}", exec_bot);
    }

    fn part_two(&self) {
        let (_, output_map) = execute(self.read_input(), [0, 0]);
        let mut result: i16 = 1;
        for (k, v) in output_map.iter() {
            if ["0", "1", "2"].contains(&k.as_str()) {
                result = result * v[0] as i16;
            }
        }

        println!("Part Two: {}", result);
    }
}

// --------------------------------------------------------------------------------------------------------------------

lazy_static! {
    static ref REG_BOT_VALUE: Regex = Regex::new(r"value (\d+) goes to bot (\d+)").unwrap();
    static ref REG_BOT_TRANSFER: Regex =
        Regex::new(r"bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)")
            .unwrap();
}

fn execute(input: String, watch_pair: [i8; 2]) -> (String, HashMap<String, Vec<i8>>) {
    let mut watched_bot: String = String::new();
    let mut bot_map: HashMap<String, Vec<i8>> = HashMap::new();
    let mut output_map: HashMap<String, Vec<i8>> = HashMap::new();
    let mut operations: HashMap<String, bool> = HashMap::new();

    for operation in input.lines() {
        operations.insert(operation.to_string(), false);
    }

    while operations.iter().any(|(_, v)| !*v) {
        for (operation, executed) in operations.iter_mut() {
            if *executed {
                continue;
            }
    
            match REG_BOT_VALUE.captures(operation) {
                Some(c) => {
                    let bot_key: &str = &c[2];
                    let value: i8 = c[1].parse().unwrap();
    
                    match bot_map.get_mut(bot_key) {
                        Some(b) => b.push(value),
                        None => {
                            bot_map.insert(String::from_str(bot_key).unwrap(), [value].to_vec());
                        }
                    }
    
                    *executed = true;
                    continue;
                }
                None => {}
            }
    
            match REG_BOT_TRANSFER.captures(operation) {
                Some(c) => {
                    let bot_key: &str = &c[1];
    
                    let target_type_1: &str = &c[2];
                    let target_key_1: &str = &c[3];
                    let target_type_2: &str = &c[4];
                    let target_key_2: &str = &c[5];
    
                    let bot_chips: Vec<i8> = match bot_map.get(bot_key) {
                        Some(chips) if chips.len() == 2 => chips.to_vec(),
                        Some(_) => {
                            // Bot has invalid amount of chips for this operation
                            continue;
                        }
                        None => {
                            // Bot not found on the map
                            continue;
                        }
                    };
                    let max_chip = bot_chips.iter().max().unwrap();
                    let min_chip = bot_chips.iter().min().unwrap();

                    if watch_pair.contains(max_chip) && watch_pair.contains(min_chip) {
                        watched_bot = String::from_str(bot_key).unwrap();
                    }
    
                    match target_type_1 {
                        // TODO: Can probably create a generic for this
                        "output" => {
                            match output_map.get_mut(target_key_1) {
                                Some(chips) => chips.push(*min_chip),
                                None => {
                                    output_map.insert(target_key_1.to_string(), [*min_chip].to_vec());
                                }
                            };
    
                        }
                        "bot" => {
                            match bot_map.get_mut(target_key_1) {
                                Some(chips) => chips.push(*min_chip),
                                None => {
                                    bot_map.insert(target_key_1.to_string(), [*min_chip].to_vec());
                                }
                            };
                        }
                        _ => panic!("Received invalid target: {}", target_type_1),
                    }
    
                    match target_type_2 {
                        "output" => {
                            match output_map.get_mut(target_key_2) {
                                Some(chips) => chips.push(*max_chip),
                                None => {
                                    output_map.insert(target_key_2.to_string(), [*max_chip].to_vec());
                                }
                            };
    
                        }
                        "bot" => {
                            match bot_map.get_mut(target_key_2) {
                                Some(chips) => chips.push(*max_chip),
                                None => {
                                    bot_map.insert(target_key_2.to_string(), [*max_chip].to_vec());
                                }
                            };
                        }
                        _ => panic!("Received invalid target: {}", target_type_2),
                    }
    
                    *executed = true;
                    continue;
                }
                None => {}
            }
        }
    }

    return (watched_bot.to_string(), output_map);
}