import sys
from collections import Counter
from typing import Dict, List, Set, Tuple

from aoc.task import Task


class Day06(Task):
    def part_one(self) -> str:
        input_data = self.read_input()
        result = run(input_data)
        return str(result)

    def part_two(self) -> str:
        return ""


def run(input_data: str) -> int:
    max_x = max_y = -sys.maxsize
    data: List[Tuple[int, int]] = []

    for line in input_data.split('\n'):
        tokens = line.split(', ')
        item = (int(tokens[0]), int(tokens[1]))
        if item[0] > max_x:
            max_x = item[0]
        if item[1] > max_y:
            max_y = item[1]

        data.append(item)

    # Expand a bit the limits and consider that to be repeating indefinitely
    grid_size = max([max_x, max_y]) + 2
    grid: List[List[int]] = [
        [-2 for _ in range(0, grid_size)]
        for _ in range(0, grid_size)
    ]
    infinite: Set[int] = set()

    for y in range(0, len(grid)):
        for x in range(0, len(grid[0])):
            # {distance: index of the bases}
            distances: Dict[int, List[int]] = {}
            for idx, base in enumerate(data):
                distance = manhattan_distance((x, y), base)
                if not distances.get(distance):
                    distances[distance] = []

                distances[distance].append(idx)

            min_distance = min(distances.items(), key=lambda item: item[0])

            if len(min_distance[1]) > 1:
                grid[y][x] = -1
                continue

            grid[y][x] = min_distance[1][0]

            # If is on the border, it will be always on the border = infinite
            if x == 0 or x == len(grid) - 1 or y == 0 or y == len(grid) - 1:
                infinite.update(min_distance[1])

    flatten_grid = [item for row in grid for item in row]
    counter = Counter(flatten_grid)
    # Remove the ones that are same distance to more than one base
    counter.pop(-1)
    # Remove the ones that are infinite
    for inf in infinite:
        counter.pop(inf, None)

    result = max(counter.items(), key=lambda item: item[1])
    return result[1]


def manhattan_distance(p1: Tuple[int, int], p2: Tuple[int, int]) -> int:
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])
