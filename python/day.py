from abc import ABC, abstractmethod

_DAY_REGISTRY = {}


class Day(ABC):
    def __init__(self, year: int, day: int):
        self.year = year
        self.day = day

    @abstractmethod
    def part_one(self) -> str:
        pass

    @abstractmethod
    def part_two(self) -> str:
        pass

    def read_input(self, input_name: str = "") -> str:
        if input_name == "":
            path = f"../inputs/{self.year}_{self.day:02}.txt"
        else:
            path = f"../inputs/{self.year}_{self.day:02}_{input_name}.txt"

        file = open(path, "r")
        return file.read()


def register_day(year: int, day: int):
    def decorator(cls):
        class BoundDay(cls):
            def __init__(self):
                super().__init__(year, day)

        _DAY_REGISTRY[f"{year}-{day}"] = BoundDay

        return cls

    return decorator


def get_day(year: int, day: int) -> Day | None:
    day_cls = _DAY_REGISTRY.get(f"{year}-{day}")
    if not day_cls:
        return None

    return day_cls()
