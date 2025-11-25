from day import Day, register_day


@register_day(2018, 2)
class Day02(Day):
    def part_one(self) -> str:
        res = [0, 0]
        for content_pairs in [
            {letter: box_id.count(letter) for letter in box_id}
            for box_id in self.read_input().split("\n")
        ]:
            # int(bool(x)) -> for any x > 0 it will result in 1
            res[0] += int(bool(len([k for k, v in content_pairs.items() if v == 2])))
            res[1] += int(bool(len([k for k, v in content_pairs.items() if v == 3])))

        result = res[0] * res[1]
        return str(result)

    def part_two(self) -> str:
        input = self.read_input().split("\n")
        result = None

        for i, bid in enumerate(input):
            for bid2 in input[i + 1 :]:
                zipped = zip(list(bid), list(bid2))
                eq = [x for x, y in zipped if x == y]
                if len(bid) - len(eq) == 1:
                    result = "".join(eq)

        return result
