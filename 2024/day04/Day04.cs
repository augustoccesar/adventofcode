using System.Collections;

class Day04 : Task
{
    public override string PartOne(string fileName)
    {
        var input = Input.ReadLines(fileName);

        int result = 0;
        for (int y = 0; y < input.Length; y++)
        {
            for (int x = 0; x < input[0].Length; x++)
            {
                if (input[y][x] == 'X')
                {
                    result += CountXmas(input, (x, y));
                }
            }
        }

        return result.ToString();
    }

    public override string PartTwo(string fileName)
    {
        return "-";
    }

    private static int CountXmas(string[] input, (int, int) from)
    {
        int count = 0;

        (int, int)[] up = [(0, -1), (0, -2), (0, -3)];
        (int, int)[] upRight = [(1, -1), (2, -2), (3, -3)];
        (int, int)[] right = [(1, 0), (2, 0), (3, 0)];
        (int, int)[] downRight = [(1, 1), (2, 2), (3, 3)];
        (int, int)[] down = [(0, 1), (0, 2), (0, 3)];
        (int, int)[] downLeft = [(-1, 1), (-2, 2), (-3, 3)];
        (int, int)[] left = [(-1, 0), (-2, 0), (-3, 0)];
        (int, int)[] upLeft = [(-1, -1), (-2, -2), (-3, -3)];

        (int, int)[][] possibleDirections = [up, upRight, right, downRight, down, downLeft, left, upLeft];

        foreach (var direction in possibleDirections)
        {
            try
            {
                char[] txt = [
                    input[from.Item2][from.Item1],
                    input[from.Item2 + direction[0].Item2][from.Item1 + direction[0].Item1],
                    input[from.Item2 + direction[1].Item2][from.Item1 + direction[1].Item1],
                    input[from.Item2 + direction[2].Item2][from.Item1 + direction[2].Item1],
            ];

                if (new string(txt) == "XMAS")
                {
                    count += 1;
                }
            }
            catch (System.IndexOutOfRangeException)
            {
                continue;
            }
        }

        return count;
    }

}
