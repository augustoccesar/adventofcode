use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

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

        for tuple in decoration.closest_junction_boxes.iter().take(1000) {
            connect_junction_boxes(&mut decoration.circuits, &tuple.0, &tuple.1);
        }

        let mut circuit_sizes = decoration
            .circuits
            .iter()
            .map(|circuit| circuit.len())
            .collect::<Vec<usize>>();
        circuit_sizes.sort();
        circuit_sizes.reverse();

        circuit_sizes.iter().take(3).product::<usize>().to_string()
    }

    fn part_two(&self) -> String {
        let input = self.read_default_input();

        let mut decoration = Decoration::parse(&input);

        let mut latest_connection = None;
        for tuple in decoration.closest_junction_boxes.iter() {
            if let Some(connection) =
                connect_junction_boxes(&mut decoration.circuits, &tuple.0, &tuple.1)
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

struct Decoration {
    circuits: Vec<Circuit>,
    closest_junction_boxes: Vec<PointTuple>,
}

impl Decoration {
    // TODO: This is probably way more complicated than it needs to be xD. But that is
    //       a problem for future me.
    fn parse(s: &str) -> Self {
        let juncion_boxes = s
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

        let mut circuits: Vec<Circuit> = Vec::new();
        for junction_box in &juncion_boxes {
            circuits.push(HashSet::from([junction_box.to_owned()]));
        }

        // Do as a HashMap first to ensure that, given a distance
        // function `d`, `d(a,b)` does not duplicate with `d(b,a)`.
        let mut distances: HashMap<PointTuple, f64> = HashMap::new();
        for i in 0..juncion_boxes.len() {
            for j in 0..juncion_boxes.len() {
                if i == j {
                    continue;
                }

                let a = &juncion_boxes[i];
                let b = &juncion_boxes[j];
                let distance = distance_between_points(a, b);

                distances.insert(PointTuple(a.clone(), b.clone()), distance);
            }
        }

        // Change distances to be a Vec now so that we can order
        let mut distances = distances.into_iter().collect::<Vec<(PointTuple, f64)>>();
        distances.sort_by(|a, b| {
            a.1.partial_cmp(&b.1)
                .expect("distance values should be comparable")
        });

        let distances = distances
            .iter()
            .map(|distance| distance.0.clone())
            .collect::<Vec<PointTuple>>();

        Self {
            circuits,
            closest_junction_boxes: distances,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Hash)]
struct Point3D {
    x: i64,
    y: i64,
    z: i64,
}

#[derive(Debug, Clone)]
struct PointTuple(Point3D, Point3D);

impl Eq for PointTuple {}
impl PartialEq for PointTuple {
    fn eq(&self, other: &Self) -> bool {
        (self.0 == other.0 && self.1 == other.1) || (self.1 == other.0 && self.0 == other.1)
    }
}

impl Hash for PointTuple {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let (first, second) = if self.0 <= self.1 {
            (&self.0, &self.1)
        } else {
            (&self.1, &self.0)
        };
        first.hash(state);
        second.hash(state);
    }
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
