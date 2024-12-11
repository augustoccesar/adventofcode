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
        return "-";
    }
}
