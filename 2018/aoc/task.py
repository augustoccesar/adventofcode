import os
from abc import ABC, abstractmethod
from typing import Optional


class Task(ABC):
    def run(self):
        print(f"Part One: {self.part_one()}")
        print(f"Part Two: {self.part_two()}")

    def read_input(self, input_name: str = "input") -> Optional[str]:
        try:
            day = self.__class__.__name__.lower()
            path = os.path.dirname(__file__)
            f = open(f"{path}/{day}/{input_name}.txt", "r")
            return f.read()
        except OSError:
            return None

    @abstractmethod
    def part_one(self) -> str:
        pass

    @abstractmethod
    def part_two(self) -> str:
        pass
