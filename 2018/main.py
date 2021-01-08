from aoc import Day01, Day02, Day03
import sys

if __name__ == "__main__":
    days = {
        "01": Day01(),
        "02": Day02(),
        "03": Day03()
    }

    if len(sys.argv) < 2:
        print("Invalid amount of argument")
        exit(1)

    day_to_run = days.get(sys.argv[1], None)
    if day_to_run is None:
        print(f"Day {sys.argv[1]} not found")
        exit(1)

    day_to_run.run()
