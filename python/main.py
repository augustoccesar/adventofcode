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
        print(day_instance.part_one())
        print(day_instance.part_two())
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
