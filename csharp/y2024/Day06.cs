using System.Diagnostics;

namespace aoc.y2024;

[RunnableDay(year: 2024, day: 6)]
class Day06 : Day
{
    public override string PartOne()
    {
        var map = ReadInputLines().Select(line => line.ToCharArray()).ToArray();
        var startingPosition = StartingPosition(map);

        var patrolResult = Patrol(map, startingPosition, Direction.Up);
        var result = patrolResult.Path.Keys.Select(p => p.Item1).ToHashSet().Count;

        return result.ToString();
    }

    public override string PartTwo()
    {
        var map = ReadInputLines().Select(line => line.ToCharArray()).ToArray();
        var startingPosition = StartingPosition(map);

        var patrolResult = Patrol(map, startingPosition, Direction.Up);
        var possibleObstacles = patrolResult.Path.Keys.Select(p => p.Item1).ToHashSet();

        var loops = 0;
        foreach (var possibleObstacle in possibleObstacles)
        {
            var newMap = map.Select(row => row.ToArray()).ToArray();
            newMap[possibleObstacle.Item2][possibleObstacle.Item1] = '#';

            var result = Patrol(newMap, startingPosition, Direction.Up);
            if (result.IsLoop)
            {
                loops++;
            }
        }

        return loops.ToString();
    }

    private static (int, int) StartingPosition(char[][] map)
    {
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

        return startPosition;
    }

    private static PatrolResult Patrol(char[][] map, (int, int) startPosition, Direction startDirection)
    {
        var trackingPositions = new Dictionary<((int, int), Direction), bool>();
        var facing = startDirection;
        var currentPosition = startPosition;
        var isLoop = false;
        while (true)
        {
            trackingPositions[(currentPosition, facing)] = true;

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
                facing = facing.TurnRight();
                continue;
            }

            if (trackingPositions.ContainsKey((nextPosition, facing)))
            {
                isLoop = true;
                break;
            }

            currentPosition = nextPosition;
        }

        return new PatrolResult(trackingPositions, isLoop);
    }
}

readonly record struct PatrolResult(Dictionary<((int, int), Direction), bool> Path, bool IsLoop);

// TODO: Move these to another file
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

    public static Direction TurnRight(this Direction direction)
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

    public static (int, int) Move(this Direction direction, (int, int) position)
    {
        var modifier = direction.Modifier();

        return (
            position.Item1 + modifier.Item1,
            position.Item2 + modifier.Item2
        );
    }
}
