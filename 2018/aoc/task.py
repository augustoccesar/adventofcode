import os
from abc import ABC, abstractmethod
from datetime import datetime


class Task(ABC):
    def run(self):
        exec_data = [
            {"header": "One", "func": self.part_one},
            {"header": "Two", "func": self.part_two}
        ]

        for data in exec_data:
            start_time = datetime.now()
            result = data["func"]()
            end_time = datetime.now()

            exec_time = ":".join(str(end_time - start_time).split(":")[1:])

            print(f"Part {data['header']}: {result} (took: {exec_time})")

    def read_input(self, input_name: str = "input") -> str:
        day = self.__class__.__name__.lower()
        path = os.path.dirname(__file__)
        f = open(f"{path}/{day}/{input_name}.txt", "r")
        return f.read()

    @abstractmethod
    def part_one(self) -> str:
        pass

    @abstractmethod
    def part_two(self) -> str:
        pass
