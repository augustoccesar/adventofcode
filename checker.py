import csv
import os

from ansible.parsing.vault import VaultLib, VaultSecret
from ansible.constants import DEFAULT_VAULT_ID_MATCH
from dataclasses import dataclass
from enum import Enum
from io import StringIO
from typing import List


class FailureType(Enum):
    WrongValue = 1
    NotFound = 2


@dataclass
class Failure:
    f_type: FailureType
    year: int
    day: int
    part: int


def print_failures(failures: List[Failure]):
    for failure in failures:
        print(
            f"{failure.year}:{failure.day} Part {failure.part} -> {failure.f_type.name}")


if __name__ == "__main__":
    valt_pass = os.environ["VAULT_PASS"]
    vault = VaultLib(
        [(DEFAULT_VAULT_ID_MATCH, VaultSecret(valt_pass.encode()))])

    available_years = [2020]
    failures = []

    for year in available_years:
        results_reader = csv.reader(
            StringIO(open(f"./{year}_results", "r").read()), delimiter=";")
        expected_results_reader = csv.reader(
            StringIO(vault.decrypt(open(f"./{year}_expected_results", "r").read()).decode("utf-8")), delimiter=";")

        results_map = {}
        expected_results_map = {}

        for row in results_reader:
            results_map[row[0]] = [row[1], row[2]]

        for row in expected_results_reader:
            expected_results_map[row[0]] = [row[1], row[2]]

        for k, v in expected_results_map.items():
            day = int(k)
            res = results_map.get(k)

            if not res:
                failures.append(Failure(FailureType.NotFound, year, day, 1))
                failures.append(Failure(FailureType.NotFound, year, day, 2))
                continue

            res = iter(res)
            part_one = next(res, None)
            part_two = next(res, None)

            if part_one is None:
                failures.append(Failure(FailureType.NotFound, year, day, 1))
            if part_two is None:
                failures.append(Failure(FailureType.NotFound, year, day, 2))

            if part_one is not None and part_one != v[0]:
                failures.append(Failure(FailureType.WrongValue, year, day, 1))

            if part_two is not None and part_two != v[1]:
                failures.append(Failure(FailureType.WrongValue, year, day, 2))

    if len(failures) > 0:
        print_failures(failures)
        exit(1)
