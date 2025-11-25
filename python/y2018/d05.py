import string
from typing import List
from day import Day, register_day


@register_day(2018, 5)
class Day05(Day):
    def part_one(self) -> str:
        data = self.read_input()
        units = list(data)

        collapsed = collapse(units)

        return str(len(collapsed))

    def part_two(self) -> str:
        data = self.read_input()

        smallest = float("inf")
        for char in list(string.ascii_lowercase):
            new_data = data.replace(char, "").replace(char.upper(), "")
            size = len(collapse(list(new_data)))
            if size < smallest:
                smallest = size

        return str(smallest)


def collapse(original_units: List[str]) -> List[str]:
    units = original_units.copy()

    i = 0
    while True:
        if i < 0:
            i = 0

        curr_unit = units[i]
        next_unit = units[i + 1]

        if is_opposite(curr_unit, next_unit):
            del units[i : i + 2]
            i -= 1
            continue

        i += 1

        if i == len(units) - 1:
            break

    return units


def is_opposite(left: str, right: str) -> bool:
    return (left.lower() == right.lower()) and (
        (left.isupper() and right.islower()) or (left.islower() and right.isupper())
    )
