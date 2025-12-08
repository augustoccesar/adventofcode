import time
import click

from day import DAY_REGISTRY, get_day

import y2018.d01
import y2018.d02
import y2018.d03
import y2018.d04
import y2018.d05
import y2018.d06
import y2018.d07
import y2018.d08
import y2018.d09
import y2018.d10
# CODEGEN:import_day


@click.group()
def cli():
    pass


@cli.command("run")
@click.argument("year")
@click.argument("day")
def run(year: int, day: int):
    day_instance = get_day(year, day)
    if day_instance:
        part_one_start = time.perf_counter_ns()
        part_one_result = day_instance.part_one()
        part_one_end = time.perf_counter_ns()
        print(f"{part_one_result};{part_one_end - part_one_start}")

        part_two_start = time.perf_counter_ns()
        part_two_result = day_instance.part_two()
        part_two_end = time.perf_counter_ns()
        print(f"{part_two_result};{part_two_end - part_two_start}")
    else:
        print(f"Day {day} not found for year {year}")

@cli.command("days")
def run():
    year_days = {}
    for (year, day) in map(lambda key: key.split("-"), DAY_REGISTRY.keys()):
        if year not in year_days:
            year_days[year] = []
        
        year_days[year].append(day)
    
    for (year, days) in year_days.items():
        print(f"{year};{";".join(days)}")


if __name__ == "__main__":
    cli()
