namespace aoc.y2024;

[RunnableDay(year: 2024, day: 7)]
class Day07 : Day
{
    public override string PartOne()
    {
        var result = 0L;

        foreach (var line in ReadInputLines())
        {
            result += CalculateEquation(line, ['+', '*']);
        }

        return result.ToString();
    }

    public override string PartTwo()
    {
        var result = 0L;

        foreach (var line in ReadInputLines())
        {
            result += CalculateEquation(line, ['+', '*', '|']);
        }

        return result.ToString();
    }

    private static long CalculateEquation(string equation, char[] availableOperators)
    {
        var allValues = equation.Split([' ', ':']).Where(c => c != "").ToList().ConvertAll(long.Parse);
        var testValue = allValues[0];
        var equationValues = allValues[1..];
        var operationsPermutations = GeneratePermutations(availableOperators, equationValues.Count - 1);

        foreach (var operationPermutation in operationsPermutations)
        {
            var acc = equationValues[0];
            for (int i = 1; i < equationValues.Count; i++)
            {
                var value = equationValues[i];
                var operation = operationPermutation[i - 1];
                switch (operation)
                {
                    case '+':
                        acc += value;
                        break;
                    case '*':
                        acc *= value;
                        break;
                    case '|':
                        acc = long.Parse(acc.ToString() + value.ToString());
                        break;
                }
            }

            if (acc == testValue)
            {
                return testValue;
            }
        }

        return 0;
    }

    private static List<string> GeneratePermutations(char[] items, int length)
    {
        List<string> result = [];

        GeneratePermutationsRecursive(items, "", length, result);

        return result;
    }

    private static void GeneratePermutationsRecursive(char[] items, string current, int length, List<string> result)
    {
        if (current.Length == length)
        {
            result.Add(current);
            return;
        }

        foreach (char item in items)
        {
            GeneratePermutationsRecursive(items, current + item, length, result);
        }
    }
}

