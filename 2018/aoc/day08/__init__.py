from __future__ import annotations

from dataclasses import dataclass
from typing import List

from aoc.task import Task


class Day08(Task):
    def part_one(self) -> str:
        license = License(self.read_input())

        return str(license.sum_metadata())

    def part_two(self) -> str:
        return "-"


@dataclass
class Node:
    children: List[Node]
    metadata: List[int]

    def sum_metadata(self) -> int:
        total = sum(self.metadata)
        if len(self.children) > 0:
            for child in self.children:
                total += child.sum_metadata()

        return total


class Reader:
    cursor: int
    inner: List[int]

    def __init__(self, data: List[int]) -> None:
        self.cursor = 0
        self.inner = data

    def read_n(self, count: int):
        data = self.inner[self.cursor:self.cursor + count]
        self.cursor += count

        return data

    def read_one(self) -> int:
        return self.read_n(1)[0]


class License:
    data: Reader
    root: Node

    def __init__(self, input_str: str) -> None:
        self.data = Reader(list(map(lambda x: int(x), input_str.split(" "))))
        self.root = self.__decode_step()

    def sum_metadata(self) -> int:
        return self.root.sum_metadata()

    def __decode_step(self) -> int:
        child_count = self.data.read_one()
        metadata_count = self.data.read_one()

        children = []
        if child_count > 0:
            for _ in range(0, child_count):
                child = self.__decode_step()
                children.append(child)

        metadata = self.data.read_n(metadata_count)

        return Node(children, metadata)
