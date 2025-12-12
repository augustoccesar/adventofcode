use itertools::Itertools;

use crate::Day;

pub struct Day10 {}

impl Day for Day10 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        10
    }

    fn part_one(&self) -> String {
        let manuals = parse_input(&self.read_default_input());

        let mut total = 0;
        for manual in &manuals {
            let mut minimal_clicks = usize::MAX;
            for pattern in &buttons_combinations(&manual.buttons) {
                let mut lights = vec![false; manual.indicator_lights.len()];

                for button in pattern {
                    for light_idx in *button {
                        lights[*light_idx] = !lights[*light_idx];
                    }
                }

                if lights == manual.indicator_lights {
                    if pattern.len() < minimal_clicks {
                        minimal_clicks = pattern.len();
                    }
                }
            }

            total += minimal_clicks;
        }

        total.to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}

#[derive(Debug)]
struct Manual {
    indicator_lights: Vec<bool>,
    buttons: Vec<Vec<usize>>,
}

fn parse_input(input: &str) -> Vec<Manual> {
    let mut manuals = vec![];

    for line in input.lines() {
        let parts = line.split(" ").collect::<Vec<_>>();
        let [indicators, schematics @ .., _joltage] = parts.as_slice() else {
            panic!("line with invalid format");
        };

        let mut indicator_lights = vec![];
        for indicator in indicators.replace(['[', ']'], "").chars() {
            match indicator {
                '.' => indicator_lights.push(false),
                '#' => indicator_lights.push(true),
                _ => panic!("invalid indicator light"),
            }
        }

        let buttons = schematics
            .iter()
            .map(|schematic| {
                schematic
                    .replace(['(', ')'], "")
                    .split(",")
                    .map(|number| {
                        number
                            .parse::<usize>()
                            .expect("wiring schematic numbers should be valid usize")
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        manuals.push(Manual {
            indicator_lights,
            buttons,
        });
    }

    manuals
}

fn buttons_combinations(buttons: &[Vec<usize>]) -> Vec<Vec<&Vec<usize>>> {
    let mut patterns = vec![];

    for buttons_count in 1..=buttons.len() {
        let pattern = buttons
            .iter()
            .combinations(buttons_count)
            .collect::<Vec<_>>();

        patterns.extend(pattern);
    }

    patterns
}
