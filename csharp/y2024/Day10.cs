namespace aoc.y2024;

using Position = (int, int);

[RunnableDay(year: 2024, day: 10)]
class Day10 : Day
{
    public override string PartOne()
    {
        var (map, trailheads) = ParseInput(ReadInputLines());

        var total = 0;
        foreach (var trailhead in trailheads)
        {
            var trails = Trails(map, trailhead);
            total += trails.ReachedEnds.Count;
        }

        return total.ToString();
    }

    public override string PartTwo()
    {
        var (map, trailheads) = ParseInput(ReadInputLines());

        var total = 0;
        foreach (var trailhead in trailheads)
        {
            var trails = Trails(map, trailhead);
            total += trails.PossibleTrails.Count;
        }

        return total.ToString();
    }

    private static ParsedInput ParseInput(string[] lines)
    {
        var map = lines
            .ToList()
            .ConvertAll(line => line.ToCharArray().ToList().Select(c => c - '0').ToList());

        List<Position> trailheads = [];
        for (int y = 0; y < map.Count; y++)
        {
            for (int x = 0; x < map[0].Count; x++)
            {
                if (map[y][x] == 0) trailheads.Add((x, y));
            }
        }

        return new ParsedInput(map, trailheads);
    }

    private static TrailsResult Trails(List<List<int>> map, Position trailhead)
    {
        List<List<Position>> trails = [];
        List<Position> pathSoFar = [trailhead];
        HashSet<Position> reachedNines = [];

        TrailsRecursive(map, pathSoFar, ref trails, ref reachedNines);

        return new TrailsResult(trails, reachedNines);
    }

    private static void TrailsRecursive(
        List<List<int>> map,
        List<Position> pathSoFar,
        ref List<List<Position>> trails,
        ref HashSet<Position> reachedNines
    )
    {
        var currPosition = pathSoFar.Last();
        if (map[currPosition.Item2][currPosition.Item1] == 9)
        {
            trails.Add(pathSoFar);
            reachedNines.Add(currPosition);
            return;
        }

        foreach (var direction in Directions())
        {
            var nextPosition = direction.Move(currPosition);
            if (!WithinBounds(map, nextPosition)) continue;

            if (map[nextPosition.Item2][nextPosition.Item1] == map[currPosition.Item2][currPosition.Item1] + 1)
            {
                pathSoFar.Add(nextPosition);
                TrailsRecursive(map, [.. pathSoFar], ref trails, ref reachedNines);
            }
        }
    }

    private static Direction[] Directions()
    {
        return [
            Direction.Up,
            Direction.Right,
            Direction.Down,
            Direction.Left,
        ];
    }

    private static bool WithinBounds(List<List<int>> map, Position position)
    {
        return position.Item1 >= 0 && position.Item1 < map[0].Count && position.Item2 >= 0 && position.Item2 < map.Count;
    }

    internal readonly record struct ParsedInput(List<List<int>> Map, List<Position> TrailHeads);

    internal readonly record struct TrailsResult(List<List<Position>> PossibleTrails, HashSet<Position> ReachedEnds);
}

