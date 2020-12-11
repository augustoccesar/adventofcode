def part_one():
    res = [0, 0]
    for content_pairs in [
        {letter: box_id.count(letter) for letter in box_id}
        for box_id
        in __read_input().split("\n")
    ]:
        # int(bool(x)) -> for any x > 0 it will result in 1
        res[0] += \
            int(bool(len([k for k, v in content_pairs.items() if v == 2])))
        res[1] += \
            int(bool(len([k for k, v in content_pairs.items() if v == 3])))

    print(f"Part One: {res[0] * res[1]}")


def part_two():
    input = __read_input().split("\n")
    result = None

    for i, bid in enumerate(input):
        for bid2 in input[i+1:]:
            zipped = zip(list(bid), list(bid2))
            eq = [x for x, y in zipped if x == y]
            if(len(bid) - len(eq) == 1):
                result = "".join(eq)

    print(f"Part Two: {result}")


# -----------------------------------------------------------------------------


def __read_input() -> str:
    try:
        f = open("./input.txt", "r")
        return f.read()
    except OSError:
        exit(1)


if __name__ == "__main__":
    part_one()
    part_two()
