def read_input() -> str:
    try:
        f = open("./input.txt", "r")
        # f = open("./example.txt", "r")
        return f.read()
    except OSError:
        exit(1)


def part_one():
    result = sum([int(x) for x in read_input().split("\n")])

    print(f"Part One: {result}")


def part_two():
    input = [int(x) for x in read_input().split("\n")]
    result = None
    frequency = 0
    mem = {}

    while result == None:
        for x in input:
            frequency += x
            if frequency in mem:
                result = frequency
                break
            else:
                mem[frequency] = 1

    print(f"Part Two: {result}")


if __name__ == "__main__":
    part_one()
    part_two()
