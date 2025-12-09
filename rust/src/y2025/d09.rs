use crate::Day;

pub struct Day09 {}

impl Day for Day09 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        9
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();
        let red_tiles_pos: Vec<Position> = parse_input(&input);

        let mut largest_area = 0;
        for i in 0..red_tiles_pos.len() {
            for j in (i + 1)..red_tiles_pos.len() {
                let tile_a = &red_tiles_pos[i];
                let tile_b = &red_tiles_pos[j];

                let fx = (tile_b.0 - tile_a.0).abs();
                let fy = (tile_b.1 - tile_a.1).abs();

                let area = (fx + 1) * (fy + 1);
                if area > largest_area {
                    largest_area = area;
                }
            }
        }

        largest_area.to_string()
    }

    fn part_two(&self) -> String {
        "-".to_owned()
    }
}

type Position = (i64, i64);

fn parse_input(input: &str) -> Vec<Position> {
    input
        .lines()
        .map(|line| {
            let [x, y] = line
                .split(",")
                .map(|number| {
                    number
                        .parse::<i64>()
                        .expect("numbers on the input should be valid i64")
                })
                .collect::<Vec<i64>>()[..]
            else {
                panic!("Invalid line format");
            };

            (x, y)
        })
        .collect::<Vec<_>>()
}
