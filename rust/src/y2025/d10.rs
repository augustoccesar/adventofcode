use itertools::Itertools;
use std::collections::HashMap;

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
            let button_idxs = (0..manual.buttons.len()).collect::<Vec<usize>>();

            for clicked_buttons_idxs in powerset(&button_idxs) {
                let mut lights = vec![false; manual.indicator_lights.len()];

                for button_idx in &clicked_buttons_idxs {
                    for light_idx in &manual.buttons[*button_idx] {
                        lights[*light_idx] = !lights[*light_idx];
                    }
                }

                if lights == manual.indicator_lights && clicked_buttons_idxs.len() < minimal_clicks
                {
                    minimal_clicks = clicked_buttons_idxs.len();
                }
            }

            total += minimal_clicks;
        }

        total.to_string()
    }

    fn part_two(&self) -> String {
        let manuals = parse_input(&self.read_default_input());

        let mut total = 0;
        for manual in manuals {
            let mut options: HashMap<Vec<i32>, Vec<Vec<usize>>> = HashMap::new();
            let mut output: HashMap<Vec<usize>, Vec<i32>> = HashMap::new();

            let button_indices: Vec<usize> = (0..manual.buttons.len()).collect();
            let powerset_result = powerset(&button_indices);

            for pressed in powerset_result {
                let supply: Vec<i32> = (0..manual.joltages.len())
                    .map(|j| {
                        pressed
                            .iter()
                            .filter(|&&b| manual.buttons[b].contains(&j))
                            .count() as i32
                    })
                    .collect();

                let parity: Vec<i32> = supply.iter().map(|&j| j % 2).collect();

                options.entry(parity).or_default().push(pressed.clone());

                output.insert(pressed, supply);
            }

            let mut cache = HashMap::new();
            let minimal_clicks =
                minimal_for_joltage(manual.joltages, &options, &output, &mut cache);

            total += minimal_clicks;
        }

        total.to_string()
    }
}

#[derive(Debug)]
struct Manual {
    indicator_lights: Vec<bool>,
    buttons: Vec<Vec<usize>>,
    joltages: Vec<i32>,
}

fn parse_input(input: &str) -> Vec<Manual> {
    let mut manuals = vec![];

    for line in input.lines() {
        let parts = line.split(" ").collect::<Vec<_>>();
        let [indicators, schematics @ .., joltage] = parts.as_slice() else {
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

        let joltages = joltage
            .replace(['{', '}'], "")
            .split(",")
            .map(|number| {
                number
                    .parse::<i32>()
                    .expect("joltage numbers should be valid i32")
            })
            .collect::<Vec<_>>();

        manuals.push(Manual {
            indicator_lights,
            buttons,
            joltages,
        });
    }

    manuals
}

fn powerset(set: &[usize]) -> Vec<Vec<usize>> {
    let sizes = 0..=set.len();

    sizes
        .flat_map(|size| {
            let combinations = set.iter().combinations(size);

            combinations.map(|combination| combination.into_iter().cloned().collect())
        })
        .collect()
}

fn minimal_for_joltage(
    joltages: Vec<i32>,
    options: &HashMap<Vec<i32>, Vec<Vec<usize>>>,
    output: &HashMap<Vec<usize>, Vec<i32>>,
    cache: &mut HashMap<Vec<i32>, usize>,
) -> usize {
    if let Some(&cached) = cache.get(&joltages) {
        return cached;
    }

    if joltages.iter().any(|&joltage| joltage.is_negative()) {
        cache.insert(joltages.clone(), usize::MAX);

        return usize::MAX;
    }

    if joltages.iter().sum::<i32>() == 0 {
        cache.insert(joltages.clone(), 0);

        return 0;
    }

    let mut minimal = usize::MAX;
    let parity: Vec<i32> = joltages.iter().map(|&j| j % 2).collect();

    if let Some(pressed_options) = options.get(&parity) {
        for pressed in pressed_options {
            if let Some(supply) = output.get(pressed) {
                let remain: Vec<i32> = joltages
                    .iter()
                    .zip(supply.iter())
                    .map(|(&joltage, &supply)| (joltage - supply) / 2)
                    .collect();

                let option_minimal = minimal_for_joltage(remain, options, output, cache);
                if option_minimal == usize::MAX {
                    continue;
                }

                let option_minimal = pressed.len() + 2 * option_minimal;
                if option_minimal < minimal {
                    minimal = option_minimal;
                }
            }
        }
    }

    cache.insert(joltages.clone(), minimal);

    minimal
}
