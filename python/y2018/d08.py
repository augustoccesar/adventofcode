from typing import List, Self
from day import Day, register_day


@register_day(2018, 8)
class Day08(Day):
    def part_one(self) -> str:
        license = License(self.read_input())

        return str(license.sum_metadata())

    def part_two(self) -> str:
        license = License(self.read_input())

        return str(license.root.value)


class Node:
    children: List[Self]
    metadata: List[int]
    value: int

    def __init__(self, children: List[Self], metadata: List[int]) -> None:
        self.children = children
        self.metadata = metadata

        if len(self.children) == 0:
            self.value = sum(self.metadata)
        else:
            total = 0
            for metadata_item in self.metadata:
                child_idx = metadata_item - 1
                if child_idx >= len(self.children) or child_idx < 0:
                    continue

                total += self.children[child_idx].value

            self.value = total

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
        data = self.inner[self.cursor : self.cursor + count]
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
