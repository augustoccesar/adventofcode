class Day00 : Task
{
    public override string PartOne(Stream fileStream)
    {
        var result = ParsedInput(fileStream).Aggregate("", (acc, item) => acc + item);

        return result.ToString();
    }

    public override string PartTwo(Stream fileStream)
    {
        var result = ParsedInput(fileStream).Aggregate(0, (acc, item) => acc + item);

        return result.ToString();
    }

    private static List<int> ParsedInput(Stream fileStream)
    {
        return Input.ReadLines(fileStream)
                    .ToList()
                    .ConvertAll(int.Parse);
    }
}
