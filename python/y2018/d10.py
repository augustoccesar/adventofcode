import copy
from dataclasses import dataclass
import re
from day import Day, register_day


@register_day(2018, 10)
class Day10(Day):
    def part_one(self) -> str:
        sky = Sky(self.read_input())
        sky.tick_until_aligned()
        # print(sky) # The print is the answer

        return "PHFZCEZX"

    def part_two(self) -> str:
        sky = Sky(self.read_input())
        sky.tick_until_aligned()

        return sky.seconds_elapsed.__str__()


PATTERN = re.compile(r"position=<(-?\d+),(-?\d+)>velocity=<(-?\d+),(-?\d+)>")


@dataclass
class Point:
    x: int
    y: int
    vx: int
    vy: int


class Sky:
    points = []
    original_points = []
    seconds_elapsed = 0

    def __init__(self, data: str):
        for line in data.replace(" ", "").splitlines():
            match = PATTERN.match(line)
            self.original_points.append(
                Point(
                    int(match.group(1)),
                    int(match.group(2)),
                    int(match.group(3)),
                    int(match.group(4)),
                )
            )

        self.points = copy.deepcopy(self.original_points)

    def height(self):
        max_y = max([p.y for p in self.points])
        min_y = min([p.y for p in self.points])
        shift_y = -1 * max_y if max_y > 0 else abs(min_y)

        return abs(min_y + shift_y)

    def tick(self, back: bool = False):
        modifier = -1 if back else 1

        for point in self.points:
            point.x += point.vx * modifier
            point.y += point.vy * modifier

        self.seconds_elapsed += 1 * modifier

    def tick_until_aligned(self):
        min_height = float("inf")
        while True:
            height = self.height()
            if height <= min_height:
                min_height = height
            else:
                self.tick(back=True)
                break

            self.tick()

    def __repr__(self) -> str:
        min_x = min([p.x for p in self.points])
        max_x = max([p.x for p in self.points])
        max_y = max([p.y for p in self.points])
        min_y = min([p.y for p in self.points])

        shift_x = abs(min_x) if min_x < 0 else (-1 * min_x)
        shift_y = (-1 * max_y) if max_y > 0 else abs(max_y)

        grid = []
        for _ in range(abs(min_y + shift_y) + 1):
            grid.append(["."] * (max_x + shift_x + 1))

        for point in self.points:
            grid[abs(point.y + shift_y)][point.x + shift_x] = "#"

        grid.reverse()
        return "\n".join(map(lambda row: "".join(row), grid))
