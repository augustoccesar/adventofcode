namespace aoc.y2024;

[RunnableDay(year: 2024, day: 4)]
class Day04 : Day
{
    public override string PartOne()
    {
        var input = ReadInputLines();

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

    public override string PartTwo()
    {
        var input = ReadInputLines();

        int result = 0;
        for (int y = 0; y < input.Length; y++)
        {
            for (int x = 0; x < input[0].Length; x++)
            {
                if (input[y][x] == 'A' && IsCrossedMas(input, (x, y)))
                {
                    result += 1;
                }
            }
        }

        return result.ToString();
    }

    private static readonly (int, int)[][] XMasModifersFromX = [
        [(0, -1), (0, -2), (0, -3)],    // Up
        [(1, -1), (2, -2), (3, -3)],    // Up Right
        [(1, 0), (2, 0), (3, 0)],       // Right
        [(1, 1), (2, 2), (3, 3)],       // Down Right
        [(0, 1), (0, 2), (0, 3)],       // Down
        [(-1, 1), (-2, 2), (-3, 3)],    // Down Left
        [(-1, 0), (-2, 0), (-3, 0)],    // Left
        [(-1, -1), (-2, -2), (-3, -3)], // Up Left
    ];

    private static readonly (int, int)[][] MasDiagonalsModifiers = [
        [(-1, -1), (1, 1)], // Top Left -> Bottom Right
        [(-1, 1), (1, -1)], // Bottom Right -> Top Right
    ];

    private static int CountXmas(string[] input, (int, int) from)
    {
        int count = 0;

        foreach (var direction in XMasModifersFromX)
        {
            try
            {
                char[] txt = [
                    input[from.Item2][from.Item1],
                    input[from.Item2 + direction[0].Item2][from.Item1 + direction[0].Item1],
                    input[from.Item2 + direction[1].Item2][from.Item1 + direction[1].Item1],
                    input[from.Item2 + direction[2].Item2][from.Item1 + direction[2].Item1],
                ];

                if (new string(txt) == "XMAS") { count += 1; }
                ;
            }
            catch (IndexOutOfRangeException) { continue; }
        }

        return count;
    }

    private static bool IsCrossedMas(string[] input, (int, int) from)
    {
        (int, int)[] diag1 = MasDiagonalsModifiers[0];
        (int, int)[] diag2 = MasDiagonalsModifiers[1];

        try
        {
            string txt1 = new([
                input[from.Item2 + diag1[0].Item2][from.Item1 + diag1[0].Item1],
                input[from.Item2][from.Item1],
                input[from.Item2 + diag1[1].Item2][from.Item1 + diag1[1].Item1],
            ]);

            string txt2 = new([
                input[from.Item2 + diag2[0].Item2][from.Item1 + diag2[0].Item1],
                input[from.Item2][from.Item1],
                input[from.Item2 + diag2[1].Item2][from.Item1 + diag2[1].Item1],
            ]);

            return (txt1 == "MAS" || txt1 == "SAM") && (txt2 == "MAS" || txt2 == "SAM");
        }
        catch (IndexOutOfRangeException) { return false; }
    }
}

