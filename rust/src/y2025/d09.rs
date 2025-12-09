use std::cmp::{max, min};

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
        let red_tiles: Vec<Point2D> = parse_input(&input);

        let mut largest_area = 0;
        for i in 0..red_tiles.len() {
            for j in (i + 1)..red_tiles.len() {
                let rect = Rectangle::from(&(&red_tiles[i], &red_tiles[j]));
                let area = area(&rect);

                if area > largest_area {
                    largest_area = area;
                }
            }
        }

        largest_area.to_string()
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();
        let red_tiles: Vec<Point2D> = parse_input(&input);

        let mut edges = Vec::new();
        for i in 0..red_tiles.len() {
            let edge = (&red_tiles[i], &red_tiles[(i + 1) % red_tiles.len()]);
            edges.push(edge);
        }

        let mut largest_area = 0;
        for i in 0..red_tiles.len() {
            for j in (i + 1)..red_tiles.len() {
                let rectangle = Rectangle::from(&(&red_tiles[i], &red_tiles[j]));
                let area = area(&rectangle);

                if area <= largest_area {
                    continue;
                }

                let mut is_cut_by_edge = false;
                for edge in &edges {
                    if is_cut_by_segment(&rectangle, edge) {
                        is_cut_by_edge = true;

                        break;
                    }
                }

                if is_cut_by_edge {
                    continue;
                }

                largest_area = area;
            }
        }

        largest_area.to_string()
    }
}

#[derive(Debug, Clone)]
struct Point2D {
    x: i64,
    y: i64,
}

#[derive(Debug)]
struct Rectangle(Point2D, Point2D);

fn area(rect: &Rectangle) -> i64 {
    let width = (rect.1.x - rect.0.x).abs() + 1;
    let height = (rect.1.y - rect.0.y).abs() + 1;

    width * height
}

fn bounds(rect: &Rectangle) -> (i64, i64, i64, i64) {
    let (min_x, max_x) = min_max(rect.0.x, rect.1.x);
    let (min_y, max_y) = min_max(rect.0.y, rect.1.y);

    (min_x, max_x, min_y, max_y)
}

fn is_cut_by_segment(rect: &Rectangle, segment: &(&Point2D, &Point2D)) -> bool {
    let (min_x, max_x, min_y, max_y) = bounds(rect);
    let (point_a, point_b) = segment;

    // Vertical
    if point_a.x == point_b.x {
        let line_x = point_a.x;

        if line_x <= min_x || line_x >= max_x {
            return false;
        }

        let (segment_min_y, segment_max_y) = min_max(point_a.y, point_b.y);

        return segment_max_y > min_y && segment_min_y < max_y;
    }

    // Horizontal
    if point_a.y == point_b.y {
        let line_y = point_a.y;

        if line_y <= min_y || line_y >= max_y {
            return false;
        }

        let (segment_min_x, segment_max_x) = min_max(point_a.x, point_b.x);

        return segment_max_x > min_x && segment_min_x < max_x;
    }

    false
}

impl From<&(&Point2D, &Point2D)> for Rectangle {
    fn from(value: &(&Point2D, &Point2D)) -> Self {
        Self(value.0.clone(), value.1.clone())
    }
}

fn parse_input(input: &str) -> Vec<Point2D> {
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

            Point2D { x, y }
        })
        .collect::<Vec<_>>()
}

fn min_max(a: i64, b: i64) -> (i64, i64) {
    (min(a, b), max(a, b))
}
