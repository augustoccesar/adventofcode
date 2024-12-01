class Day01 : Task
{
    public override string PartOne(string fileName)
    {
        var lists = new List<int>[2] { [], [] };

        foreach (var line in ParseInput(fileName))
        {
            lists[0].Add(line[0]);
            lists[1].Add(line[1]);
        }

        lists[0].Sort();
        lists[1].Sort();

        var res = 0;
        for (int i = 0; i < lists[0].Count; i++)
        {
            res += Math.Abs(lists[0][i] - lists[1][i]);
        }

        return res.ToString();
    }

    public override string PartTwo(string fileName)
    {
        var list = new List<int>();
        var occurenceDict = new Dictionary<int, int>();

        foreach (var line in ParseInput(fileName))
        {
            list.Add(line[0]);

            if (occurenceDict.TryGetValue(line[1], out int value))
            {
                occurenceDict[line[1]] = value + 1;
            }
            else
            {
                occurenceDict[line[1]] = 1;
            }
        }

        var res = list
            .Select(item => item * occurenceDict.GetValueOrDefault(item, 0))
            .Sum();

        return res.ToString();
    }

    private static List<List<int>> ParseInput(string fileName)
    {
        return Input
            .ReadLines(fileName)
            .Select(line =>
                line.Split("   ")
                    .ToList()
                    .ConvertAll(int.Parse)
            )
            .ToList();
    }
}
