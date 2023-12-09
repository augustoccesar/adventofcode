import sys

from aoc import (
    Day01,
    Day02,
    Day03,
    Day04,
    Day05,
    Day06,
    Day07,
    Day08,
    Day09,
    Day10,
# SETUP:target_import
)

if __name__ == "__main__":
    days = {
        1: Day01(),
        2: Day02(),
        3: Day03(),
        4: Day04(),
        5: Day05(),
        6: Day06(),
        7: Day07(),
        8: Day08(),
        9: Day09(),
        10: Day10(),
# SETUP:target_dict
    }

    if len(sys.argv) < 2:
        print("Invalid amount of argument")
        exit(1)

    day_arg = int(sys.argv[1])
    day_to_run = days.get(day_arg, None)
    if day_to_run is None:
        print(f"Day {day_arg} not found")
        exit(1)

    day_to_run.run()
