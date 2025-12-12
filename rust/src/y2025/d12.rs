use crate::Day;

pub struct Day12 {}

impl Day for Day12 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        12
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();
        let (regions, presents) = parse_input(&input);

        let mut result = 0;
        for region in regions {
            let region_area = region.width * region.height;
            let required_area = region
                .presents_count
                .iter()
                .enumerate()
                .map(|(present_idx, count)| presents[present_idx].area * count)
                .sum::<usize>();

            if required_area <= region_area {
                result += 1;
            }
        }

        result.to_string()
    }

    fn part_two(&self) -> String {
        "-".to_string()
    }
}

#[derive(Debug)]
struct Region {
    width: usize,
    height: usize,
    presents_count: [usize; 6],
}

struct Present {
    _shape: Vec<bool>,
    area: usize,
}

fn parse_input(input: &str) -> (Vec<Region>, Vec<Present>) {
    let sections = input.split("\n\n").collect::<Vec<_>>();
    let (regions_input, presents_input) = sections.split_last().unwrap();

    let presents = presents_input
        .iter()
        .map(|present_raw| {
            let shape = present_raw
                .lines()
                .skip(1)
                .flat_map(|line| line.chars().map(|ch| ch == '#').collect::<Vec<_>>())
                .collect::<Vec<bool>>();

            let area = shape.iter().filter(|a| **a).count();

            Present {
                _shape: shape,
                area,
            }
        })
        .collect::<Vec<Present>>();

    let mut regions = vec![];
    for region_raw in regions_input.lines() {
        let (dimensions, counts) = region_raw.split_once(':').unwrap();

        let (width, height) = dimensions
            .split_once('x')
            .map(|(width, height)| {
                (
                    width.parse::<usize>().unwrap(),
                    height.parse::<usize>().unwrap(),
                )
            })
            .unwrap();

        let presents_count: [usize; 6] = counts
            .trim()
            .split(" ")
            .map(|count| count.parse::<usize>().unwrap())
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        regions.push(Region {
            width,
            height,
            presents_count,
        });
    }

    (regions, presents)
}
