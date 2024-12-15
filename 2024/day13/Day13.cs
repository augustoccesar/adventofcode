class Day13 : Task
{
    public override string PartOne(string fileName)
    {
        long tokens = 0;
        foreach (var machine in ReadMachines(fileName, 0))
        {
            if (Winnable(machine, out var price))
            {
                tokens += price;
            }
        }

        return tokens.ToString();
    }
    public override string PartTwo(string fileName)
    {
        long tokens = 0;
        foreach (var machine in ReadMachines(fileName, 10_000_000_000_000))
        {
            if (Winnable(machine, out var price))
            {
                tokens += price;
            }
        }

        return tokens.ToString();
    }

    private static List<Machine> ReadMachines(string fileName, long addedPrizePos)
    {
        var lines = Input.ReadLines(fileName);
        List<Machine> machines = [];
        for (int i = 0; i < lines.Length; i++)
        {
            var buttonALine = lines[i];
            var buttonALineTokens = buttonALine.Split(separators).ToList();

            var buttonBLine = lines[i + 1];
            var buttonBLineTokens = buttonBLine.Split(separators).ToList();

            var prizeLine = lines[i + 2];
            var prizeLineTokens = prizeLine.Split(separators).ToList();

            if (i % 4 == 0)
            {
                machines.Add(new Machine(
                    new Button(int.Parse(buttonALineTokens[2]), int.Parse(buttonALineTokens[4])),
                    new Button(int.Parse(buttonBLineTokens[2]), int.Parse(buttonBLineTokens[4])),
                    new Position(int.Parse(prizeLineTokens[2]) + addedPrizePos, int.Parse(prizeLineTokens[4]) + addedPrizePos)
                ));

                i += 3;
            }
        }

        return machines;
    }

    private static bool Winnable(Machine machine, out long price)
    {
        price = 0;

        var determinant = machine.A.MovX * machine.B.MovY - machine.B.MovX * machine.A.MovY;
        var a = (machine.Prize.X * machine.B.MovY - machine.B.MovX * machine.Prize.Y) / determinant;
        var b = (machine.A.MovX * machine.Prize.Y - machine.Prize.X * machine.A.MovY) / determinant;

        var x = a * machine.A.MovX + b * machine.B.MovX;
        var y = a * machine.A.MovY + b * machine.B.MovY;

        if (x == machine.Prize.X && y == machine.Prize.Y)
        {
            price = (a * 3) + (b * 1);
            return true;
        }

        return false;
    }

    private static readonly char[] separators = [':', ',', '+', '='];

    private readonly record struct Position(long X, long Y);
    private readonly record struct Button(int MovX, int MovY);
    private readonly record struct Machine(Button A, Button B, Position Prize);
}
