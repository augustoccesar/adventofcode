class Day00 : Task
{
    public override string PartOne(string fileName)
    {
        var result = ParsedInput(fileName).Aggregate("", (acc, item) => acc + item);

        return result.ToString();
    }

    public override string PartTwo(string fileName)
    {
        var result = ParsedInput(fileName).Aggregate(0, (acc, item) => acc + item);

        return result.ToString();
    }

    private static List<int> ParsedInput(string fileName)
    {
        return Input.ReadLines(fileName)
                    .ToList()
                    .ConvertAll(int.Parse);
    }
}
