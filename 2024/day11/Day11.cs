class Day11 : Task
{
    public override string PartOne(string fileName)
    {
        var stones = Input.ReadString(fileName).Split(" ").Select(long.Parse).ToList();

        for (int blinks = 0; blinks < 25; blinks++)
        {
            var newStones = new List<long>(stones.Count * 2);
            foreach (var stone in stones)
            {
                if (stone == 0)
                {
                    newStones.Add(1);
                }
                else if (stone.ToString().Length % 2 == 0)
                {
                    var stoneStr = stone.ToString();
                    var left = stoneStr[0..(stoneStr.Length / 2)];
                    var right = stoneStr[(stoneStr.Length / 2)..];

                    newStones.Add(long.Parse(left));
                    newStones.Add(long.Parse(right));
                }
                else
                {
                    newStones.Add(stone * 2024);
                }
            }

            stones = newStones;
        }

        return stones.Count.ToString();
    }

    public override string PartTwo(string fileName)
    {
        var stones = Input.ReadString(fileName).Split(" ").Select(long.Parse).ToList();
        var stonesMap = new Dictionary<long, long>();
        foreach (var stone in stones)
        {
            InsertOrIncrement(ref stonesMap, stone, 1);
        }

        for (int blinks = 0; blinks < 75; blinks++)
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

            stonesMap = newStonesMap;
        }

        return stonesMap.Values.Sum().ToString();
    }

    private static void InsertOrIncrement(ref Dictionary<long, long> dict, long key, long value)
    {
        if (!dict.TryAdd(key, value))
        {
            dict[key] += value;
        }
    }
}