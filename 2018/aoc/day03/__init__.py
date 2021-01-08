import re
from collections import Counter
from dataclasses import dataclass
from typing import Tuple, List, Dict

from aoc.task import Task


class Day03(Task):
    def part_one(self) -> str:
        claims = parse_input(self.read_input())
        claims_points = []
        for c in claims:
            for cp in c.area():
                claims_points.append(cp)

        res = len([k for k, v in Counter(claims_points).items() if v >= 2])

        return str(res)

    def part_two(self) -> str:
        claims = parse_input(self.read_input())
        points_map: Dict[Tuple[int, int], List[int]] = dict()
        intersect = []

        claim_ids = [c.identifier for c in claims]

        for c in claims:
            for cp in c.area():
                if points_map.get(cp) is not None:
                    points_map[cp].append(c.identifier)
                    intersect.extend(points_map[cp])
                else:
                    points_map[cp] = [c.identifier]

        res = set(claim_ids) - set(intersect)

        return str(res.pop())


# -----------------------------------------------------------------------------

claim_pattern = re.compile(r'#(\d+)\s@\s(\d+,\d+):\s(\d+x\d+)')


@dataclass
class Claim:
    identifier: int
    starting_pos: Tuple[int, int]
    size: Tuple[int, int]

    def area(self) -> List[Tuple[int, int]]:
        taken = []
        x = self.starting_pos[0]
        y = self.starting_pos[1]

        for yi in range(0, self.size[1]):
            for xi in range(0, self.size[0]):
                taken.append((x + xi, y + yi))

        return taken


def parse_input(input_data: str) -> List[Claim]:
    claims = []
    claims_raw = claim_pattern.findall(input_data)

    for rc in claims_raw:
        pos = list(map(lambda x: int(x), rc[1].split(",")))
        size = list(map(lambda x: int(x), rc[2].split("x")))
        claims.append(
            Claim(int(rc[0]), (pos[0], pos[1]), (size[0], size[1]))
        )

    return claims
