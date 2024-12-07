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
        return "-";
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
