class Day13 : Task
{
    public override string PartOne(string fileName)
    {
        var machines = ReadMachines(fileName);

        var tokens = 0;
        foreach (var machine in machines)
        {
            var solution = (0, 0);
            var cheapestSolution = int.MaxValue;
            for (int a = 0; a < 100; a++)
            {
                for (int b = 0; b < 100; b++)
                {
                    var x = a * machine.A.MovX + b * machine.B.MovX;
                    var y = a * machine.A.MovY + b * machine.B.MovY;
                    var price = (a * 3) + (b * 1);

                    if (x == machine.Prize.X && y == machine.Prize.Y)
                    {
                        if (price < cheapestSolution)
                        {
                            solution = (a, b);
                            cheapestSolution = price;
                        }
                    }
                }
            }

            if (solution != (0, 0))
            {
                tokens += cheapestSolution;
            }
        }

        return tokens.ToString();
    }

    public override string PartTwo(string fileName)
    {
        return "-";
    }

    private static List<Machine> ReadMachines(string fileName)
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
                    new Position(int.Parse(prizeLineTokens[2]), int.Parse(prizeLineTokens[4]))
                ));

                i += 3;
            }
        }

        return machines;
    }

    private static readonly char[] separators = [':', ',', '+', '='];

    private readonly record struct Position(int X, int Y);
    private readonly record struct Button(int MovX, int MovY);
    private readonly record struct Machine(Button A, Button B, Position Prize);
}
