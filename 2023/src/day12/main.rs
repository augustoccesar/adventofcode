use std::collections::HashMap;

use aoc2023::{read_input, timed};

fn part_one() -> String {
    let sum = read_input("12")
        .lines()
        .map(|line| {
            let (spring, groups) = line.split_once(' ').unwrap();
            (
                spring.as_bytes(),
                groups
                    .split(',')
                    .map(|group| group.parse::<usize>().unwrap())
                    .collect::<Vec<usize>>(),
            )
        })
        .map(|(spring, groups)| {
            let mut cache: HashMap<String, usize> = HashMap::new();
            count_options(spring, None, &groups, &mut cache)
        })
        .sum::<usize>();

    sum.to_string()
}

fn part_two() -> String {
    let sum = read_input("12")
        .lines()
        .map(|line| {
            let (spring, groups) = line.split_once(' ').unwrap();
            let spring = format!("{spring}?{spring}?{spring}?{spring}?{spring}");
            let groups = format!("{groups},{groups},{groups},{groups},{groups}");

            (
                spring,
                groups
                    .split(',')
                    .map(|group| group.parse::<usize>().unwrap())
                    .collect::<Vec<usize>>(),
            )
        })
        .map(|(spring, groups)| {
            let mut cache: HashMap<String, usize> = HashMap::new();
            count_options(spring.as_bytes(), None, &groups, &mut cache)
        })
        .sum::<usize>();

    sum.to_string()
}

fn main() {
    timed(part_one);
    timed(part_two);
}

fn count_options(
    springs: &[u8],
    current_group_size: Option<usize>,
    groups: &[usize],
    cache: &mut HashMap<String, usize>,
) -> usize {
    let cache_key = format!("{:?}{:?}{:?}", springs, current_group_size, groups);
    if cache.contains_key(&cache_key) {
        return *cache.get(&cache_key).unwrap();
    }

    if springs.is_empty() {
        let result = match current_group_size {
            Some(group_size) if groups == [group_size] => 1,
            None if groups.is_empty() => 1,
            _ => 0,
        };

        cache.insert(cache_key, result);
        return result;
    }

    let result = match (springs[0], current_group_size, groups) {
        // Found '.', is outside of group: just move on
        (b'.', None, _) => count_options(&springs[1..], None, groups, cache),

        // Found '?', is outside of group and there are no groups left: just move on
        (b'?', None, []) => count_options(&springs[1..], None, groups, cache),

        // Found '.' or '?', is inside a group and the group is the size of the current group: get out of group and remove the group from the list
        (b'.' | b'?', Some(current_size), [group_size, ..]) if current_size == *group_size => {
            count_options(&springs[1..], None, &groups[1..], cache)
        }

        // Found '#' or `?`, is inside a group and the current size is smaller than the current group: increment the current size and move on
        (b'#' | b'?', Some(current_size), [group_size, ..]) if current_size < *group_size => {
            count_options(&springs[1..], Some(current_size + 1), groups, cache)
        }

        // Found `#` outside of a group and there are groups to solve: increment group size and move on
        (b'#', None, [_, ..]) => count_options(&springs[1..], Some(1), groups, cache),

        // Found `?`, outside of a group, and there are groups to solve: split into two different, one inside a group (to cover for '#')
        // and one outside of a group (to cover for `.`)
        (b'?', None, _) => {
            count_options(&springs[1..], None, groups, cache)
                + count_options(&springs[1..], Some(1), groups, cache)
        }
        _ => 0,
    };

    cache.insert(cache_key, result);

    result
}
