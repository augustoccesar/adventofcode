import operator
import re

from collections import Counter
from itertools import chain
from typing import Dict, Tuple, List

from aoc.task import Task

event_pattern = re.compile(r"\[(\d+-\d+-\d+\s\d+:(\d+))\]\s(Guard|falls|wakes)\s(?:#(\d+))?")


class Day04(Task):
    def part_one(self) -> str:
        data = "\n".join(sorted(self.read_input().splitlines()))
        registry = parse_data(data)

        sleepy_id: int = max(guards_sleep_time(registry).items(), key=operator.itemgetter(1))[0]
        highest_minute: int = guard_highest_minute_occurrence(registry[sleepy_id])

        return str(sleepy_id * highest_minute)

    def part_two(self) -> str:
        return str("Not implemented.")


def guard_highest_minute_occurrence(registry_ranges: List[Tuple[int, int]]) -> int:
    counter = Counter(chain.from_iterable(tuples_to_ranges(registry_ranges)))
    max_occurrence = max(counter.items(), key=operator.itemgetter(1))
    return max_occurrence[0]


def guards_sleep_time(registry: Dict[int, List[Tuple[int, int]]]) -> Dict[int, int]:
    # kv                                                            -> Iter item from items
    # f1.       map(lambda x: x[1] - x[0], kv[1])                   -> Get the diff of the times for every guard
    # f2.       map(lambda kv: (kv[0], sum(f1)), registry.items())  -> Map the guard ID to the sum of time diffs
    # return.   dict(f2)                                            -> Collect the result of as a dict (from the tuples)
    #          |--------------------- f2 -------------------------------------------------------|
    #                                     |--------------- f1 -------------|
    return dict(map(lambda kv: (kv[0], sum(map(lambda x: x[1] - x[0], kv[1]))), registry.items()))


def tuples_to_ranges(t: List[Tuple[int, int]]) -> List[range]:
    return list(map(lambda y: range(y[0], y[1]), t))


def parse_data(data: str) -> Dict[int, List[Tuple[int, int]]]:
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
