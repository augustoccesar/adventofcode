using System.Diagnostics;

class Day06 : Task
{
    public override string PartOne(string fileName)
    {
        var map = Input.ReadLines(fileName);
        var startPosition = (-1, -1);
        for (int y = 0; y < map.Length && startPosition == (-1, -1); y++)
        {
            for (int x = 0; x < map[0].Length && startPosition == (-1, -1); x++)
            {
                if (map[y][x] == '^')
                {
                    startPosition = (x, y);
                    break;
                }
            }
        }

        var trackingPositions = new Dictionary<(int, int), bool>();
        var facing = Direction.Up;
        var currentPosition = startPosition;
        while (true)
        {
            trackingPositions[currentPosition] = true;

            var directionModifier = facing.Modifier();
            var nextPosition = (
                currentPosition.Item1 + directionModifier.Item1,
                currentPosition.Item2 + directionModifier.Item2
            );

            if (
                nextPosition.Item1 < 0 || nextPosition.Item1 >= map[0].Length
                || nextPosition.Item2 < 0 || nextPosition.Item2 >= map.Length
            )
            {
                break;
            }

            if (map[nextPosition.Item2][nextPosition.Item1] == '#')
            {
                facing = facing.Turn90Right();
                continue;
            }

            currentPosition = nextPosition;
        }

        return trackingPositions.Count.ToString();
    }

    public override string PartTwo(string fileName)
    {
        return "-";
    }
}

enum Direction
{
    Up,
    Right,
    Down,
    Left
}

static class Extensions
{
    public static (int, int) Modifier(this Direction direction)
    {
        return direction switch
        {
            Direction.Up => (0, -1),
            Direction.Right => (1, 0),
            Direction.Down => (0, 1),
            Direction.Left => (-1, 0),
            _ => throw new UnreachableException(),
        };
    }

    public static Direction Turn90Right(this Direction direction)
    {
        return direction switch
        {
            Direction.Up => Direction.Right,
            Direction.Right => Direction.Down,
            Direction.Down => Direction.Left,
            Direction.Left => Direction.Up,
            _ => throw new UnreachableException(),
        };
    }
}
