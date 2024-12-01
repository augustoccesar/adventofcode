class Day01 : Task
{
    public override string PartOne(string fileName)
    {
        var lists = new List<int>[2] { [], [] };

        foreach (var line in Input.ReadLines(fileName).Select(line => line.Split("   ").ToList().ConvertAll(int.Parse)))
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
        return "-";
    }
}
