use aoc2023::{read_input, timed};

fn part_one() -> String {
    read_input("15")
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(hash)
        .sum::<u32>()
        .to_string()
}

fn part_two() -> String {
    let mut boxes: Vec<Vec<(String, u8)>> = vec![Vec::new(); 256];

    read_input("15")
        .lines()
        .next()
        .unwrap()
        .split(',')
        .for_each(|item| {
            if item.contains('-') {
                let tokens = item.split('-').collect::<Vec<_>>();
                let label = tokens[0];
                let box_index = hash(label) as usize;

                for (i, (key, _)) in boxes[box_index].iter().enumerate() {
                    if key == label {
                        boxes[box_index].remove(i);
                        break;
                    }
                }
            } else if item.contains('=') {
                let tokens = item.split('=').collect::<Vec<_>>();
                let label = tokens[0];
                let box_index = hash(label) as usize;
                let focal_length = tokens[1].parse::<u8>().unwrap();

                let mut replaced = false;
                for (i, (key, _)) in boxes[box_index].iter().enumerate() {
                    if key == label {
                        boxes[box_index][i].1 = focal_length;
                        replaced = true;
                        break;
                    }
                }

                if !replaced {
                    boxes[box_index].push((label.to_string(), focal_length));
                }
            } else {
                unreachable!()
            };
        });

    let mut lenses_focus_power = 0;
    for (box_index, r#box) in boxes.iter().enumerate() {
        for (slot_index, lens) in r#box.iter().enumerate() {
            let focusing_power = (box_index + 1) * (slot_index + 1) * lens.1 as usize;
            lenses_focus_power += focusing_power;
        }
    }

    lenses_focus_power.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn hash(input: &str) -> u32 {
    let chars = input.chars().map(|c| c as u32).collect::<Vec<u32>>();

    let mut current_value = 0;
    for c in chars {
        current_value += c;
        current_value *= 17;
        current_value %= 256;
    }

    current_value
}
