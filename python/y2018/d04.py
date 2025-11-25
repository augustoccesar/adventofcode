from itertools import chain
import operator
import re
from typing import Counter, Dict, List, Tuple
from day import Day, register_day


@register_day(2018, 4)
class Day04(Day):
    def part_one(self) -> str:
        registry = parse_input(self.read_input())

        sleepy_id: int = max(
            guards_sleep_time(registry).items(), key=operator.itemgetter(1)
        )[0]
        highest_minute: int = guard_highest_minute_occurrence(registry[sleepy_id])

        return str(sleepy_id * highest_minute)

    def part_two(self) -> str:
        registry = parse_input(self.read_input())

        times = {k: guard_times_occurrences(v) for k, v in registry.items()}
        guards_each_max = {
            k: max(v.items(), key=operator.itemgetter(1)) for k, v in times.items()
        }
        max_of_all = max(
            guards_each_max.items(), key=lambda item: item[1][1]
        )  # (guard_id, (minute, amount))

        return str(max_of_all[0] * max_of_all[1][0])


def guard_highest_minute_occurrence(registry_ranges: List[Tuple[int, int]]) -> int:
    """
    Get the minute with highest occurrence from the registry of a guard
    :param registry_ranges: List of ranges for sleep time of a guard
    :return: Minute with highest occurrence
    """
    return max(
        guard_times_occurrences(registry_ranges).items(), key=operator.itemgetter(1)
    )[0]


def guard_times_occurrences(registry_ranges: List[Tuple[int, int]]) -> Dict[int, int]:
    """
    Get the occurrence of each minute by the ranges on the registry
    :param registry_ranges: List of ranges for sleep time of a guard
    :return: Dict mapping each minute to the occurrence of it
    """
    return Counter(chain.from_iterable(tuples_to_ranges(registry_ranges)))


def guards_sleep_time(registry: Dict[int, List[Tuple[int, int]]]) -> Dict[int, int]:
    # kv                                                            -> Iter item from items
    # f1.       map(lambda x: x[1] - x[0], kv[1])                   -> Get the diff of the times for every guard
    # f2.       map(lambda kv: (kv[0], sum(f1)), registry.items())  -> Map the guard ID to the sum of time diffs
    # return.   dict(f2)                                            -> Collect the result of as a dict (from the tuples)
    #          |--------------------- f2 -------------------------------------------------------|
    #                                     |--------------- f1 -------------|
    return dict(
        map(
            lambda kv: (kv[0], sum(map(lambda x: x[1] - x[0], kv[1]))), registry.items()
        )
    )


def tuples_to_ranges(t: List[Tuple[int, int]]) -> List[range]:
    """
    Transform list of tuples of integers into list of iterable ranges
    :param t: List of tuples to transform into list of ranges
    :return: List of ranges
    """
    return list(map(lambda y: range(y[0], y[1]), t))


def parse_input(data: str) -> Dict[int, List[Tuple[int, int]]]:
    """
    Parse the input data into the defined structure of a registry.
    A registry is a dict of a guard id to a list of tuples that represent the range of minutes on which
    the guard was asleep
    :param data: Input data string
    :return: Representation of the registry
    """
    data = "\n".join(sorted(data.splitlines()))

    event_pattern = re.compile(
        r"\[(\d+-\d+-\d+\s\d+:(\d+))\]\s(Guard|falls|wakes)\s(?:#(\d+))?"
    )

    registry: Dict[int, List[Tuple[int, int]]] = {}

    curr_guard, curr_time_start, curr_time_end = -1, -1, -1
    for match in event_pattern.finditer(data):
        if match.group(3) == "Guard":
            curr_guard = int(match.group(4))
            continue

        if match.group(3) == "falls":
            curr_time_start = int(match.group(2))
            continue

        if match.group(3) == "wakes":
            curr_time_end = int(match.group(2))
            if registry.get(curr_guard, None) is not None:
                registry[curr_guard].append((curr_time_start, curr_time_end))
            else:
                registry[curr_guard] = [(curr_time_start, curr_time_end)]
            continue

    return registry
