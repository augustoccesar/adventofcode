import sys
from typing import Any, Counter, Dict, Iterator, List, Optional, Set, Tuple
from day import Day, register_day


@register_day(2018, 6)
class Day06(Day):
    result: Optional[Tuple[int, int]] = None

    def part_one(self) -> str:
        input_data = self.read_input()
        safe_size, _ = self.__execute(input_data)

        return str(safe_size)

    def part_two(self) -> str:
        input_data = self.read_input()
        _, region_size = self.__execute(input_data)

        return str(region_size)

    def __execute(self, input_str: str) -> Tuple[int, int]:
        MAX_TOTAL_DISTANCE = 10_000

        if self.result is None:
            self.result = run(input_str, MAX_TOTAL_DISTANCE)

        return self.result


# -----------------------------------------------------------------------------

Grid = List[List[Any]]
Point = Tuple[int, int]


def run(input_data: str, max_total_distance: int) -> Tuple[int, int]:
    data, max_x, max_y = parse_input(input_data)

    # All the points that have a total distance smaller than max_total_distance
    region_size: int = 0

    # Expand a bit the limits and consider that to be repeating indefinitely
    grid_size = max([max_x, max_y]) + 2
    grid: Grid = build_grid(grid_size, -2)
    infinite: Set[int] = set()

    for x, y, _ in iterate_grid(grid):
        distances: Dict[int, List[int]] = {}  # {distance: index of the bases}
        min_distance: Tuple[int, List[int]] = (sys.maxsize, [])
        total_distance: int = 0

        # Calculate and aggregate distance from all points to the bases
        for idx, base in enumerate(data):
            distance = manhattan_distance((x, y), base)
            if not distances.get(distance):
                distances[distance] = []

            distances[distance].append(idx)

        # Find out the shortest distance and the bases that are in that distance.
        # Also take this loop to aggregate into the total distance from this point
        for distance, bases in distances.items():
            total_distance += distance * len(bases)

            if distance < min_distance[0]:
                min_distance = (distance, bases)

        # In case this point has a total distance smaller than the maximum accepted,
        # increment to the region size
        if total_distance < max_total_distance:
            region_size += 1

        # In case there is more than one base with the same min distance from this
        # point, use -1 to identify that
        if len(min_distance[1]) > 1:
            grid[y][x] = -1
            continue

        # Use the index of the base to identify the closest base from this point
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
    return result[1], region_size


def parse_input(input_str: str) -> Tuple[List[Point], int, int]:
    """
    Returns:
        Tuple[List[Point], int, int]: Parsed points, Max x-axis, Max y-axis
    """
    max_x = max_y = -sys.maxsize
    data: List[Tuple[int, int]] = []

    for line in input_str.split("\n"):
        tokens = line.split(", ")
        item = (int(tokens[0]), int(tokens[1]))
        if item[0] > max_x:
            max_x = item[0]
        if item[1] > max_y:
            max_y = item[1]

        data.append(item)

    return data, max_x, max_y


def build_grid(size: int, default: Any = None) -> Grid:
    return [[default for _ in range(0, size)] for _ in range(0, size)]


def iterate_grid(grid: List[List[Any]]) -> Iterator[Tuple[int, int, Any]]:
    """
    Returns:
        Iterator[Tuple[int, int, Any]]: Iterator returning: (current x-axis, current y-axis, item in current position)
    """
    for y in range(0, len(grid)):
        for x in range(0, len(grid[0])):
            yield x, y, grid[y][x]


def manhattan_distance(p1: Point, p2: Point) -> int:
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])
