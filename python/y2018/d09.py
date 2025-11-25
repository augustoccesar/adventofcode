import re
from typing import Self
from day import Day, register_day


@register_day(2018, 9)
class Day09(Day):
    def part_one(self) -> str:
        match = PATTERN.match(self.read_input())
        players = int(match.group(1))
        last_marble = int(match.group(2))

        winner_points = self.__run_game(players, last_marble)
        return str(winner_points)

    # TODO: This is running very slow. Should try to optmize it.
    def part_two(self) -> str:
        match = PATTERN.match(self.read_input())
        players = int(match.group(1))
        last_marble = int(match.group(2)) * 100

        winner_points = self.__run_game(players, last_marble)
        return str(winner_points)

    def __run_game(self, players: int, last_marble: int) -> int:
        points = [0] * players
        circle = Circle()
        current_marble = 1
        while current_marble <= last_marble:
            for player in range(0, players):
                award_points = circle.insert(current_marble)
                points[player] += award_points
                current_marble += 1

                if current_marble > last_marble:
                    break

        return max(points)


PATTERN = re.compile(r"(\d+) players; last marble is worth (\d+) points")


class Node:
    value: int
    previous: Self
    next: Self


class Circle:
    current_node: Node

    def __init__(self) -> None:
        root = Node()
        root.value = 0
        root.previous = root
        root.next = root

        self.current_node = root

    def remove_node(self, node: Node):
        temp_prev = node.previous
        temp_next = node.next

        temp_prev.next = temp_next
        temp_next.previous = temp_prev

    def insert(self, marble: int) -> int:
        if marble % 23 == 0:
            points = marble
            extract_node = self.current_node
            for _ in range(0, 7):
                extract_node = extract_node.previous

            points += extract_node.value
            self.remove_node(extract_node)
            self.current_node = extract_node.next

            return points

        node = Node()
        node.value = marble

        node_1 = self.current_node.next
        node_2 = node_1.next

        node.previous = node_1
        node.next = node_2

        node_1.next = node
        node_2.previous = node

        self.current_node = node

        return 0
