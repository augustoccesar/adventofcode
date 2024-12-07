class Day07 : Task
{
    public override string PartOne(string fileName)
    {
        var result = 0L;

        foreach (var line in Input.ReadLines(fileName))
        {
            var allValues = line.Split([' ', ':']).Where(c => c != "").ToList().ConvertAll(long.Parse);
            var testValue = allValues[0];
            var equationValues = allValues[1..];
            var operationsPermutations = Permutations(equationValues.Count - 1);
            foreach (var operationPermutation in operationsPermutations)
            {
                var acc = equationValues[0];
                for (int i = 1; i < equationValues.Count; i++)
                {
                    var value = equationValues[i];
                    var operationMask = 1 << (i - 1);
                    if ((operationPermutation & operationMask) > 0)
                    {
                        acc *= value;
                    }
                    else
                    {
                        acc += value;
                    }
                }

                if (acc == testValue)
                {
                    result += testValue;
                    break;
                }
            }
        }

        return result.ToString();
    }

    public override string PartTwo(string fileName)
    {
        var result = 0L;

        foreach (var line in Input.ReadLines(fileName))
        {
            var allValues = line.Split([' ', ':']).Where(c => c != "").ToList().ConvertAll(long.Parse);
            var testValue = allValues[0];
            var equationValues = allValues[1..];
            var operationsPermutations = GeneratePermutations(['*', '+', '|'], equationValues.Count - 1);

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
                    result += testValue;
                    break;
                }
            }
        }

        return result.ToString();
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

    private static List<int> Permutations(int count)
    {
        List<int> permutations = [];
        int maxValue = ~(~0 << count);
        permutations.Add(maxValue);

        int currentValue = 0;
        while (currentValue < maxValue)
        {
            permutations.Add(currentValue);
            currentValue++;
        }

        return permutations;
    }
}
