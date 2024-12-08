class Day08 : Task
{
    public override string PartOne(string fileName)
    {
        var map = Input.ReadLines(fileName).Select(line => line.ToCharArray()).ToArray();
        var antenasLocations = new Dictionary<char, (int, int)[]>();
        for (var y = 0; y < map.Length; y++)
        {
            for (var x = 0; x < map[0].Length; x++)
            {
                if (map[y][x] != '.')
                {
                    if (antenasLocations.TryGetValue(map[y][x], out var locations))
                    {
                        antenasLocations[map[y][x]] = [.. locations, (x, y)];
                    }
                    else
                    {
                        antenasLocations[map[y][x]] = [(x, y)];
                    }
                }
            }
        }

        var antinodesPositions = new HashSet<(int, int)>();
        foreach (var entry in antenasLocations)
        {
            if (entry.Value.Length > 1)
            {
                for (var i = 0; i < entry.Value.Length; i++)
                {
                    for (var j = i + 1; j < entry.Value.Length; j++)
                    {
                        var antinode1 = AntinodeOf(entry.Value[i], entry.Value[j]);
                        if (IsPositionInRange(antinode1, map))
                        {
                            antinodesPositions.Add(antinode1);
                        }

                        var antinode2 = AntinodeOf(entry.Value[j], entry.Value[i]);
                        if (IsPositionInRange(antinode2, map))
                        {
                            antinodesPositions.Add(antinode2);
                        }
                    }
                }
            }
        }

        return antinodesPositions.Count.ToString();
    }

    public override string PartTwo(string fileName)
    {
        return "-";
    }

    private static bool IsPositionInRange((int, int) position, char[][] map)
    {
        return position.Item1 >= 0 && position.Item1 < map[0].Length && position.Item2 >= 0 && position.Item2 < map.Length;
    }

    private static (int, int) AntinodeOf((int, int) source, (int, int) target)
    {
        var modX = source.Item1 - target.Item1;
        var modY = source.Item2 - target.Item2;

        return (source.Item1 + modX, source.Item2 + modY);
    }
}
