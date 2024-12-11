class Day11 : Task
{
    public override string PartOne(string fileName)
    {
        return Blink(fileName, 25).Values.Sum().ToString();
    }

    public override string PartTwo(string fileName)
    {
        return Blink(fileName, 75).Values.Sum().ToString();
    }

    private static Dictionary<long, long> BuildStonesMap(string inputName)
    {
        var stones = Input.ReadString(inputName).Split(" ").Select(long.Parse).ToList();
        var stonesMap = new Dictionary<long, long>();
        foreach (var stone in stones)
        {
            InsertOrIncrement(ref stonesMap, stone, 1);
        }

        return stonesMap;
    }

    private static Dictionary<long, long> Blink(string inputName, int times)
    {
        var stonesMap = BuildStonesMap(inputName);
        for (int i = 0; i < times; i++)
        {
            stonesMap = IterateStonesMap(stonesMap);
        }
        return stonesMap;
    }

    private static Dictionary<long, long> IterateStonesMap(Dictionary<long, long> stonesMap)
    {
        var newStonesMap = new Dictionary<long, long>();
        foreach (var (stone, count) in stonesMap)
        {
            if (stone == 0)
            {
                InsertOrIncrement(ref newStonesMap, 1, count);
            }
            else if (stone.ToString().Length % 2 == 0)
            {
                var stoneStr = stone.ToString();
                var left = long.Parse(stoneStr[0..(stoneStr.Length / 2)]);
                var right = long.Parse(stoneStr[(stoneStr.Length / 2)..]);

                InsertOrIncrement(ref newStonesMap, left, count);
                InsertOrIncrement(ref newStonesMap, right, count);
            }
            else
            {
                InsertOrIncrement(ref newStonesMap, stone * 2024, count);
            }
        }

        return newStonesMap;
    }

    private static void InsertOrIncrement(ref Dictionary<long, long> dict, long key, long value)
    {
        if (!dict.TryAdd(key, value))
        {
            dict[key] += value;
        }
    }
}
