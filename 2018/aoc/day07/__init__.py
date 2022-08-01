from __future__ import annotations

import re
import string
from dataclasses import dataclass
from enum import Enum
from typing import Dict, List, Optional

from aoc.task import Task

PATTERN = re.compile(
    r"Step (\w) must be finished before step (\w) can begin\.")


class Day07(Task):
    def part_one(self) -> str:
        workpool = WorkPool(self.read_input(), 1)
        while not all([v is WorkStatus.DONE for _, v in workpool.statuses.items()]):
            workpool.tick()

        return "".join(workpool.work_done)

    def part_two(self) -> str:
        workpool = WorkPool(self.read_input(), 5)
        while not all([v is WorkStatus.DONE for _, v in workpool.statuses.items()]):
            workpool.tick()

        return str(workpool.curr_tick - 1)

# -----------------------------------------------------------------------------


@dataclass
class Worker:
    current_work: Optional[str]
    started_at: Optional[int]

    def default() -> Worker:
        return Worker(None, None)


class WorkStatus(Enum):
    TODO = 1
    IN_PROGRESS = 2
    DONE = 3


class WorkPool:
    curr_tick: int
    dependencies: Dict[str, List[str]]
    statuses: Dict[str, WorkStatus]
    time_requirements: Dict[str, int]
    workers: List[Worker]
    work_done: List[str]

    def __init__(self, input: str, workers_count: int, added_time=0) -> None:
        self.curr_tick = 0
        self.dependencies = {}
        self.statuses = {}
        self.workers = []
        self.work_done = []

        # `ord` - Return the Unicode code point for a one-character string.
        # ord('A') == 65
        # ord('A') - 64 == 1
        # ord('B') - 64 == 2
        # (...)
        self.time_requirements = {
            k: (ord(k) - 64) + 60 for k in string.ascii_uppercase}

        for line in input.split("\n"):
            match = PATTERN.match(line)
            required = match.group(1)
            step = match.group(2)

            # Add requirements for step
            if self.dependencies.get(step) is None:
                self.dependencies[step] = []
            if self.dependencies.get(required) is None:
                self.dependencies[required] = []

            self.dependencies[step].append(required)

            # Add statuses for new steps
            if self.statuses.get(required) is None:
                self.statuses[required] = WorkStatus.TODO
            if self.statuses.get(step) is None:
                self.statuses[step] = WorkStatus.TODO

        for _ in range(workers_count):
            self.workers.append(Worker.default())

    def available_work(self) -> List[str]:
        available_items = []
        for item, _ in dict(filter(lambda item: item[1] is WorkStatus.TODO, self.statuses.items())).items():
            if len(self.dependencies[item]) == 0:
                available_items.append(item)
                continue

            statuses = [v is WorkStatus.DONE for k,
                        v in self.statuses.items() if k in self.dependencies[item]]
            if all(statuses):
                available_items.append(item)

        available_items.sort()  # TODO: Is this necessary?
        return available_items

    def tick(self) -> None:
        available_work = self.available_work()

        for worker in self.workers:
            if worker.current_work is not None:
                time_required = self.time_requirements[worker.current_work]
                if self.curr_tick - worker.started_at == time_required:
                    self.statuses[worker.current_work] = WorkStatus.DONE
                    self.work_done.append(worker.current_work)
                    available_work = self.available_work()

                    if len(available_work) > 0:
                        work = available_work.pop(0)
                        worker.current_work = work
                        worker.started_at = self.curr_tick
                        self.statuses[work] = WorkStatus.IN_PROGRESS
                    else:
                        worker.current_work = None
                        worker.started_at = None
            else:
                if len(available_work) > 0:
                    work = available_work.pop(0)
                    worker.current_work = work
                    worker.started_at = self.curr_tick
                    self.statuses[work] = WorkStatus.IN_PROGRESS

        self.curr_tick += 1
