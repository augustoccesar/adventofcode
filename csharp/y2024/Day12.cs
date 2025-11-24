namespace aoc.y2024;

using Position = (int, int);

[RunnableDay(year: 2024, day: 12)]
class Day12 : Day
{
    public override string PartOne()
    {
        var input = ReadInputLines();
        char[][] garden = new char[input.Length][];
        var plotsMap = new Dictionary<Position, char>();
        for (var y = 0; y < input.Length; y++)
        {
            garden[y] = [.. input[y]];
            for (var x = 0; x < garden[y].Length; x++)
            {
                plotsMap[(x, y)] = garden[y][x];
            }
        }

        List<Dictionary<Position, int>> regions = [];
        while (plotsMap.Count > 0)
        {
            Dictionary<Position, int> region = [];
            var currentPos = plotsMap.First().Key;
            plotsMap.Remove(currentPos);

            var currentType = garden[currentPos.Item2][currentPos.Item1];
            region.Add(currentPos, currentType);

            Stack<Position> frontier = [];
            frontier.Push(currentPos);

            while (frontier.Count > 0)
            {
                var position = frontier.Pop();
                var fences = 0;
                foreach (var direction in Directions())
                {
                    var neighbor = direction.Move(position);

                    if (region.ContainsKey(neighbor)) continue;

                    if (!WithinBounds(garden, neighbor))
                    {
                        fences++;
                        continue;
                    }
                    ;

                    if (currentType == garden[neighbor.Item2][neighbor.Item1])
                    {
                        region.Add(neighbor, 0);
                        frontier.Push(neighbor);
                        plotsMap.Remove(neighbor);
                    }
                    else
                    {
                        fences++;
                    }
                }

                region[position] = fences;
            }

            regions.Add(region);
        }

        var total = 0;
        foreach (var area in regions)
        {
            total += area.Count * area.Values.Sum();
        }

        return total.ToString();
    }

    public override string PartTwo()
    {
        return "-";
    }

    // TODO: Move this to a shared place since is used by day 10 as well
    private static Direction[] Directions()
    {
        return [
            Direction.Up,
            Direction.Right,
            Direction.Down,
            Direction.Left,
        ];
    }

    // TODO: Move this to a shared place since is used by day 10 as well
    private static bool WithinBounds(char[][] map, Position position)
    {
        return position.Item1 >= 0 && position.Item1 < map[0].Length && position.Item2 >= 0 && position.Item2 < map.Length;
    }
}

