from aoc.task import Task


class Day05(Task):
    def part_one(self) -> str:
        data = self.read_input()
        units = list(data)

        i = 0
        while True:
            if i < 0:
                i = 0

            curr_unit = units[i]
            next_unit = units[i + 1]

            if is_opposite(curr_unit, next_unit):
                del units[i:i + 2]
                i -= 1
                continue

            i += 1

            # Do this by the end of while
            if i == len(units) - 1:
                break

        print(units)

        return str(len(units))

    def part_two(self) -> str:
        return "Not Implemented"


def is_opposite(left: str, right: str) -> bool:
    return (left.lower() == right.lower()) \
           and ((left.isupper() and right.islower()) or (left.islower() and right.isupper()))
