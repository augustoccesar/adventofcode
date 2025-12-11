use std::collections::HashMap;

use crate::Day;

pub struct Day11 {}

impl Day for Day11 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        11
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();

        let mut devices: HashMap<&str, Vec<&str>> = HashMap::new();

        for line in input.lines() {
            let [device, output, ..] = line.split(':').collect::<Vec<_>>()[..] else {
                panic!("invalid formatted line");
            };

            let output_devices = output.trim().split(' ').collect::<Vec<_>>();

            devices.insert(device, output_devices);
        }

        fn lookup(devices: &HashMap<&str, Vec<&str>>, from: &str) -> usize {
            let mut total = 0;
            for output in devices
                .get(from)
                .expect("'from' should be in devices registry")
            {
                if *output == "out" {
                    total += 1;
                } else {
                    total += lookup(devices, output)
                }
            }

            total
        }

        lookup(&devices, "you").to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}
