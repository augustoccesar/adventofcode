from aoc.task import Task


class Day01(Task):
    def part_one(self) -> str:
        return str(sum([int(x) for x in self.read_input().split("\n")]))

    def part_two(self) -> str:
        input = [int(x) for x in self.read_input().split("\n")]
        result = None
        frequency = 0
        mem = {}

        while result is None:
            for x in input:
                frequency += x
                if frequency in mem:
                    result = frequency
                    break
                else:
                    mem[frequency] = 1

        return str(result)
