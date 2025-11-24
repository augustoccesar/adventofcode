import click

from day import get_day

import y2018.d01
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


if __name__ == "__main__":
    cli()
