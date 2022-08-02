from __future__ import annotations

import re

from aoc.task import Task

PATTERN = re.compile(
    r"(\d+) players; last marble is worth (\d+) points")


class Node:
    value: int
    previous: Node
    next: Node


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


class Day09(Task):
    def part_one(self) -> str:
        match = PATTERN.match(self.read_input())
        players = int(match.group(1))
        last_marble = int(match.group(2))

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

        winner_points = max(points)
        return str(winner_points)

    # TODO: This is running very slow. Should try to optmize it.
    #   Took 20 seconds-ish (not that should probably add the timer to the 2018 as well).
    def part_two(self) -> str:
        match = PATTERN.match(self.read_input())
        players = int(match.group(1))
        last_marble = int(match.group(2)) * 100

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

        winner_points = max(points)
        return str(winner_points)
