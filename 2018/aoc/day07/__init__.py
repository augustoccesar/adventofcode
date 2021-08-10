from typing import Dict, List
from aoc.task import Task
import re

PATTERN = re.compile(
    r"Step (\w) must be finished before step (\w) can begin\.")


class Day07(Task):
    def part_one(self) -> str:
        requirements: Dict[str, List[str]] = {}
        statuses: Dict[str, bool] = {}
        instruction: List[str] = []

        for line in self.read_input().split("\n"):
            match = PATTERN.match(line)
            required = match.group(1)
            step = match.group(2)

            # Add requirements for step
            if requirements.get(step) is None:
                requirements[step] = []

            requirements[step].append(required)

            # Add statuses for new steps
            if statuses.get(required) is None:
                statuses[required] = False
            if statuses.get(step) is None:
                statuses[step] = False

        # Check the first items of the instruction
        # A.K.A. doesn't have a requirement
        roots = [
            item
            for item in statuses.keys()
            if item not in requirements.keys()
        ]

        for root in roots:
            requirements[root] = []

        # Loop through the statuses items until all of them are enabled
        while any([status == False for status in statuses.values()]):
            activable_in_loop: List[str] = []
            for item, _ in {k: v for k, v in statuses.items() if v is False}.items():
                if can_activate(item, requirements, statuses):
                    activable_in_loop.append(item)

            to_activate = sorted(activable_in_loop)[0]
            instruction.append(to_activate)
            statuses[to_activate] = True

        return "".join(instruction)

    def part_two(self) -> str:
        return "-"

# -----------------------------------------------------------------------------


def can_activate(item: str, requirements: Dict[str, List[str]], statuses: Dict[str, bool]) -> bool:
    req: List[str] = requirements[item]
    for req_item in req:
        # If any of the requirements is not finished, return false
        if statuses[req_item] is False:
            return False

    return True
