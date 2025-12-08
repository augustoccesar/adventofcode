use std::{cmp, collections::HashSet, hash::Hash};

use crate::Day;

pub struct Day08 {}

impl Day for Day08 {
    fn year(&self) -> u16 {
        2025
    }

    fn day(&self) -> u8 {
        8
    }

    fn part_one(&self) -> String {
        let input = self.read_default_input();
        let mut decoration = Decoration::parse(&input);

        for pair in decoration.closest_junction_boxes.iter().take(1000) {
            connect_junction_boxes(&mut decoration.circuits, &pair.0, &pair.1);
        }

        let mut circuit_sizes = decoration
            .circuits
            .iter()
            .map(|circuit| circuit.len())
            .collect::<Vec<usize>>();

        circuit_sizes.sort_by_key(|key| cmp::Reverse(*key));

        circuit_sizes.iter().take(3).product::<usize>().to_string()
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();
        let mut decoration = Decoration::parse(&input);

        let mut latest_connection = None;
        for pair in decoration.closest_junction_boxes.iter() {
            if let Some(connection) =
                connect_junction_boxes(&mut decoration.circuits, &pair.0, &pair.1)
            {
                latest_connection = Some(connection);
            }
        }

        let latest_connection =
            latest_connection.expect("there should have been a latest connection");

        (latest_connection.0.x * latest_connection.1.x).to_string()
    }
}

type JunctionBox = Point3D;
type Circuit = HashSet<JunctionBox>;
type JunctionBoxPair = (JunctionBox, JunctionBox);

struct Decoration {
    circuits: Vec<Circuit>,
    closest_junction_boxes: Vec<JunctionBoxPair>,
}

impl Decoration {
    fn parse(s: &str) -> Self {
        let junction_boxes = s
            .lines()
            .map(|line| {
                let mut coordinates = line.split(",").map(|number_str| {
                    number_str
                        .parse::<i64>()
                        .expect("number should be valid i64")
                });

                JunctionBox {
                    x: coordinates
                        .next()
                        .expect("coordinates should have a first value for 'x'"),
                    y: coordinates
                        .next()
                        .expect("coordinates should have a first value for 'y'"),
                    z: coordinates
                        .next()
                        .expect("coordinates should have a first value for 'z'"),
                }
            })
            .collect::<Vec<JunctionBox>>();

        let mut circuits = Vec::with_capacity(junction_boxes.len());
        let mut distances: Vec<(JunctionBoxPair, f64)> = Vec::new();

        for (i, junction_box) in junction_boxes.iter().enumerate() {
            circuits.push(HashSet::from([junction_box.clone()]));

            for other in junction_boxes.iter().take(i) {
                let distance = distance_between_points(junction_box, other);

                distances.push(((junction_box.clone(), other.clone()), distance));
            }
        }

        let mut closest_junction_boxes = distances;

        closest_junction_boxes.sort_by(|a, b| {
            a.1.partial_cmp(&b.1)
                .expect("distance values should be comparable")
        });

        let closest_junction_boxes = closest_junction_boxes
            .into_iter()
            .map(|(pair, _)| pair)
            .collect();

        Self {
            circuits,
            closest_junction_boxes,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Point3D {
    x: i64,
    y: i64,
    z: i64,
}

fn connect_junction_boxes<'a>(
    circuits: &mut Vec<Circuit>,
    jb1: &'a JunctionBox,
    jb2: &'a JunctionBox,
) -> Option<(&'a JunctionBox, &'a JunctionBox)> {
    let mut from_index: Option<usize> = None;
    let mut to_index: Option<usize> = None;

    // Find the index of the circuit of both junction boxes
    for (i, circuit) in circuits.iter().enumerate() {
        if from_index.is_some() && to_index.is_some() {
            break;
        }

        if circuit.contains(jb1) {
            from_index = Some(i);
        }

        if circuit.contains(jb2) {
            to_index = Some(i);
        }
    }

    let from_idx = from_index.expect("from_circuit should be found");
    let to_idx = to_index.expect("to_circuit should be found");

    // They are already on the same circuit
    if from_idx == to_idx {
        return None;
    }

    // Since we are merging two different indexes of the same vector and we don't care
    // about the order, make sure to remove the largest one to avoid shifting the vector.
    let (smaller_idx, larger_idx) = if from_idx < to_idx {
        (from_idx, to_idx)
    } else {
        (to_idx, from_idx)
    };

    // Pop the largest index one to extend to the smallest index one
    let circuit_to_merge = circuits.remove(larger_idx);
    circuits[smaller_idx].extend(circuit_to_merge);

    Some((jb1, jb2))
}

fn distance_between_points(a: &Point3D, b: &Point3D) -> f64 {
    // Euclidean distance
    // ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2) ^ 1/2
    (((b.x - a.x).pow(2) + (b.y - a.y).pow(2) + (b.z - a.z).pow(2)) as f64).sqrt()
}
