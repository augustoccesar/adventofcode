class Day00 : Task
{
    public override string PartOne(string fileName)
    {
        var result = Input.ReadLinesAs(fileName, int.Parse).Aggregate("", (acc, item) => acc + item);

        return result.ToString();
    }

    public override string PartTwo(string fileName)
    {
        var result = Input.ReadLinesAs(fileName, int.Parse).Aggregate(0, (acc, item) => acc + item);

        return result.ToString();
    }
}
