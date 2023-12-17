use std::collections::{hash_map::Entry, BinaryHeap, HashMap};

use aoc2023::{read_input, timed};

fn part_one() -> String {
    let map = parse_map(&read_input("17"));

    let factory_pos = (map[0].len() - 1, map.len() - 1);
    let mut distance_cache: HashMap<NodeDistanceCacheKey, usize> = HashMap::new();
    let mut queue: BinaryHeap<Node> = BinaryHeap::new();

    queue.push(Node {
        cost: 0,
        position: (0, 0),
        direction: Direction::East,
        steps_same_direction: 0,
    });
    queue.push(Node {
        cost: 0,
        position: (0, 0),
        direction: Direction::South,
        steps_same_direction: 0,
    });

    while let Some(node) = queue.pop() {
        if node.position == factory_pos {
            return node.cost.to_string();
        }

        match distance_cache.entry(NodeDistanceCacheKey::from(&node)) {
            Entry::Occupied(entry) if node.cost > *entry.get() => continue,
            _ => (),
        }

        for next_direction in node.direction.iter_without_opposite() {
            let modifier = next_direction.modifier();
            let next_position = (
                node.position.0 as i32 + modifier.0,
                node.position.1 as i32 + modifier.1,
            );

            if next_position.0 >= map[0].len() as i32
                || next_position.1 >= map.len() as i32
                || next_position.0 < 0
                || next_position.1 < 0
            {
                continue;
            }

            let next_position = (next_position.0 as usize, next_position.1 as usize);
            let next_node = Node {
                position: next_position,
                direction: next_direction,
                steps_same_direction: if next_direction == node.direction {
                    node.steps_same_direction + 1
                } else {
                    1
                },
                cost: node.cost + map[next_position.1][next_position.0],
            };

            let next_node_cache_key = NodeDistanceCacheKey::from(&next_node);
            if next_node.steps_same_direction <= 3
                && (!distance_cache.contains_key(&next_node_cache_key)
                    || next_node.cost < distance_cache[&next_node_cache_key])
            {
                distance_cache.insert(next_node_cache_key, next_node.cost);

                queue.push(next_node);
            }
        }
    }

    unreachable!()
}

fn part_two() -> String {
    let map = parse_map(&read_input("17"));

    let factory_pos = (map[0].len() - 1, map.len() - 1);
    let mut distance_cache: HashMap<NodeDistanceCacheKey, usize> = HashMap::new();

    let mut queue = BinaryHeap::new();

    queue.push(Node {
        cost: 0,
        position: (0, 0),
        direction: Direction::East,
        steps_same_direction: 0,
    });
    queue.push(Node {
        cost: 0,
        position: (0, 0),
        direction: Direction::South,
        steps_same_direction: 0,
    });

    while let Some(node) = queue.pop() {
        if node.position == factory_pos && node.steps_same_direction >= 4 {
            return node.cost.to_string();
        }

        match distance_cache.entry(NodeDistanceCacheKey::from(&node)) {
            Entry::Occupied(entry) if node.cost > *entry.get() => continue,
            _ => (),
        }

        for next_direction in node.direction.iter_without_opposite() {
            let modifier = next_direction.modifier();
            let next_position = (
                node.position.0 as i32 + modifier.0,
                node.position.1 as i32 + modifier.1,
            );

            if next_position.0 >= map[0].len() as i32
                || next_position.1 >= map.len() as i32
                || next_position.0 < 0
                || next_position.1 < 0
            {
                continue;
            }

            let next_position = (next_position.0 as usize, next_position.1 as usize);
            let next_node = Node {
                position: next_position,
                direction: next_direction,
                steps_same_direction: if next_direction == node.direction {
                    node.steps_same_direction + 1
                } else {
                    1
                },
                cost: node.cost + map[next_position.1][next_position.0],
            };

            let next_node_cache_key = NodeDistanceCacheKey::from(&next_node);
            if (node.direction == next_node.direction || node.steps_same_direction >= 4)
                && next_node.steps_same_direction <= 10
                && (!distance_cache.contains_key(&next_node_cache_key)
                    || next_node.cost < distance_cache[&next_node_cache_key])
            {
                distance_cache.insert(next_node_cache_key, next_node.cost);
                queue.push(next_node);
            }
        }
    }

    unreachable!()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn parse_map(input: &str) -> Vec<Vec<usize>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|char| char.to_digit(10).unwrap() as usize)
                .collect::<Vec<usize>>()
        })
        .collect::<Vec<Vec<usize>>>()
}

#[derive(Eq, PartialEq)]
struct Node {
    cost: usize,
    position: (usize, usize),
    direction: Direction,
    steps_same_direction: usize,
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.position.cmp(&other.position))
    }
}

#[derive(Eq, PartialEq, Hash)]
struct NodeDistanceCacheKey {
    position: (usize, usize),
    direction: Direction,
    steps_same_direction: usize,
}

impl From<&Node> for NodeDistanceCacheKey {
    fn from(value: &Node) -> Self {
        NodeDistanceCacheKey {
            position: value.position,
            direction: value.direction,
            steps_same_direction: value.steps_same_direction,
        }
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Hash)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    const fn modifier(&self) -> (i32, i32) {
        match self {
            Direction::North => (0, -1),
            Direction::East => (1, 0),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
        }
    }

    const fn opposite(&self) -> Self {
        match self {
            Direction::North => Direction::South,
            Direction::East => Direction::West,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
        }
    }

    fn iter_without_opposite(&self) -> impl Iterator<Item = Direction> + '_ {
        vec![
            Direction::North,
            Direction::West,
            Direction::South,
            Direction::East,
        ]
        .into_iter()
        .filter(move |&direction| direction != self.opposite())
    }
}
