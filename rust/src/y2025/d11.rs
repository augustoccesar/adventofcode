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
        let devices = parse_input(&input);

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
        let input = self.read_default_input();
        let devices = parse_input(&input);

        let mut cache: HashMap<(&str, bool, bool), usize> = HashMap::new();

        fn lookup<'device>(
            devices: &HashMap<&'device str, Vec<&'device str>>,
            cache: &mut HashMap<(&'device str, bool, bool), usize>,
            from: &'device str,
            dac: bool,
            fft: bool,
        ) -> usize {
            if let Some(a) = cache.get(&(from, dac, fft)) {
                return *a;
            }

            let dac = dac || from == "dac";
            let fft = fft || from == "fft";

            let mut total = 0;
            for output in devices
                .get(from)
                .expect("'from' should be in devices registry")
            {
                if *output == "out" {
                    if dac && fft {
                        total += 1;
                    }
                } else {
                    total += lookup(devices, cache, output, dac, fft);
                }
            }

            cache.insert((from, dac, fft), total);

            total
        }

        lookup(&devices, &mut cache, "svr", false, false).to_string()
    }
}

fn parse_input(input: &str) -> HashMap<&str, Vec<&str>> {
    let mut devices: HashMap<&str, Vec<&str>> = HashMap::new();

    for line in input.lines() {
        let [device, output, ..] = line.split(':').collect::<Vec<_>>()[..] else {
            panic!("invalid formatted line");
        };

        let output_devices = output.trim().split(' ').collect::<Vec<_>>();

        devices.insert(device, output_devices);
    }

    devices
}
