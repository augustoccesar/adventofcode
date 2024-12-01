use aoc2023::{read_input, read_named_input, timed};

#[derive(Debug)]
enum Category {
    X,
    M,
    A,
    S,
}

impl Category {
    fn parse(input: char) -> Self {
        match input {
            'x' => Self::X,
            'm' => Self::M,
            'a' => Self::A,
            's' => Self::S,
            _ => panic!("Unknown category {}", input),
        }
    }
}

#[derive(Debug)]
struct Part {
    x: u16,
    m: u16,
    a: u16,
    s: u16,
}

impl Part {
    fn parse(input: &str) -> Self {
        let input = &input[1..input.len() - 1];
        let mut part = Self {
            x: 0,
            m: 0,
            a: 0,
            s: 0,
        };

        for mut category_pair in input.split(',').map(|it| it.split('=')) {
            let category = category_pair.next().unwrap();
            let value = category_pair.next().unwrap().parse::<u16>().unwrap();

            match category {
                "x" => part.x = value,
                "m" => part.m = value,
                "a" => part.a = value,
                "s" => part.s = value,
                _ => panic!("Unknown category {} for part", category),
            }
        }

        return part;
    }

    fn rating(&self) -> u16 {
        return self.x + self.m + self.a + self.s;
    }
}

#[derive(Debug)]
enum Cmp {
    Lte,
    Gte,
}

impl Cmp {
    fn parse(input: char) -> Self {
        match input {
            '>' => Self::Gte,
            '<' => Self::Lte,
            _ => panic!("Unknown comparison {}", input),
        }
    }

    fn evaluate(&self, left: u16, right: u16) -> bool {
        match self {
            Cmp::Lte => left < right,
            Cmp::Gte => left > right,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Outcome {
    Accepted,
    Rejected,
}

#[derive(Debug)]
enum Destination {
    Workflow(String),
    Outcome(Outcome),
}

impl Destination {
    fn parse(input: &str) -> Self {
        match input {
            "A" => Self::Outcome(Outcome::Accepted),
            "R" => Self::Outcome(Outcome::Rejected),
            workflow => Self::Workflow(String::from(workflow)),
        }
    }
}

#[derive(Debug)]
enum Rule {
    Value {
        category: Category,
        cmp: Cmp,
        value: u16,
        destination: Destination,
    },
    Destination {
        destination: Destination,
    },
}

impl Rule {
    fn parse(input: &str) -> Self {
        let cmp_position = input.chars().position(|x| x == '<' || x == '>');

        if cmp_position.is_some() {
            let mut parts = input.split(&['<', '>', ':']);

            return Self::Value {
                category: Category::parse(parts.next().unwrap().chars().next().unwrap()),
                cmp: Cmp::parse(input.chars().nth(cmp_position.unwrap()).unwrap()),
                value: parts.next().unwrap().parse::<u16>().unwrap(),
                destination: Destination::parse(parts.next().unwrap()),
            };
        } else {
            return Self::Destination {
                destination: Destination::parse(input),
            };
        }
    }

    fn apply(&self, part: &Part) -> Option<&Destination> {
        match self {
            Rule::Value {
                category,
                cmp,
                value,
                destination,
            } => {
                let eval = match category {
                    Category::X => cmp.evaluate(part.x, *value),
                    Category::M => cmp.evaluate(part.m, *value),
                    Category::A => cmp.evaluate(part.a, *value),
                    Category::S => cmp.evaluate(part.s, *value),
                };

                if eval {
                    return Some(destination);
                } else {
                    return None;
                }
            }
            Rule::Destination { destination } => Some(destination),
        }
    }
}

#[derive(Debug)]
struct Workflow {
    name: String,
    rules: Vec<Rule>,
}

impl Workflow {
    fn parse(input: &str) -> Self {
        let mut p = input.split("{");
        let name = p.next().unwrap();
        let rules_raw = p.next().unwrap();
        let rules: Vec<Rule> = rules_raw[0..rules_raw.len() - 1]
            .split(",")
            .map(|rr| Rule::parse(rr))
            .collect();

        return Self {
            name: String::from(name),
            rules,
        };
    }

    fn run(&self, part: &Part) -> &Destination {
        for rule in &self.rules {
            match rule.apply(part) {
                Some(destination) => return destination,
                None => (),
            }
        }

        unreachable!("No rules were matched");
    }
}

fn find_workflow<'a>(name: &'a str, workflows: &'a Vec<Workflow>) -> Option<&'a Workflow> {
    for workflow in workflows {
        if workflow.name == name {
            return Some(workflow);
        }
    }

    return None;
}

fn part_one() -> String {
    let input = read_input("19");
    let input_parts: Vec<&str> = input.split("\n\n").collect();
    let workflows: Vec<Workflow> = input_parts[0].split("\n").map(Workflow::parse).collect();
    let parts: Vec<Part> = input_parts[1].split("\n").map(Part::parse).collect();

    let initial_workflow = find_workflow("in", &workflows);
    let mut accepted_parts: Vec<&Part> = vec![];
    for part in &parts {
        let mut next_workflow = initial_workflow;
        while next_workflow.is_some() {
            match next_workflow.unwrap().run(&part) {
                Destination::Workflow(workflow) => {
                    next_workflow = Some(find_workflow(workflow, &workflows).unwrap())
                }
                Destination::Outcome(outcome) => {
                    if *outcome == Outcome::Accepted {
                        accepted_parts.push(&part);
                    }
                    next_workflow = None;
                }
            }
        }
    }

    let res = accepted_parts
        .into_iter()
        .fold(0u64, |acc, x| acc + x.rating() as u64);

    res.to_string()
}

fn part_two() -> String {
    String::from("part two")
}

fn main() {
    timed(part_one);
    timed(part_two);
}
