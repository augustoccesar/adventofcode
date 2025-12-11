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
            total += find_min_clicks(manual, vec![false; manual.indicator_lights.len()], 0, 1);
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

fn find_min_clicks(
    manual: &Manual,
    current_lights: Vec<bool>,
    clicking_idx: usize,
    clicks_so_far: usize,
) -> usize {
    for i in clicking_idx..manual.buttons.len() {
        let button = &manual.buttons[i];

        let mut test_lights = current_lights.clone();
        for toggling_light in button {
            test_lights[*toggling_light] = !test_lights[*toggling_light];
        }

        if test_lights
            .iter()
            .enumerate()
            .all(|(i, light)| *light == manual.indicator_lights[i])
        {
            return clicks_so_far;
        }
    }

    let mut min_clicks = usize::MAX;
    for i in clicking_idx..manual.buttons.len() {
        let button = &manual.buttons[i];

        let mut test_lights = current_lights.clone();
        for toggling_light in button {
            test_lights[*toggling_light] = !test_lights[*toggling_light];
        }

        let new_clicks = find_min_clicks(manual, test_lights, i + 1, clicks_so_far + 1);
        if new_clicks < min_clicks {
            min_clicks = new_clicks;
        }
    }

    min_clicks
}
