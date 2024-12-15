using System.Diagnostics;

class Day15 : Task
{
    public override string PartOne(string fileName)
    {
        var input = ParseInput(fileName);

        var currPosition = input.StartingPos;

        for (var i = 0; i < input.Movements.Length; i++)
        {
            currPosition = MoveRobot(input.Map, currPosition, input.Movements[i]);
        }

        var total = GpsSum(input.Map);

        return total.ToString();
    }

    public override string PartTwo(string fileName)
    {
        return "-";
    }

    internal static ParsedInput ParseInput(string inputName)
    {
        var input = Input.ReadString(inputName).Split("\n\n");
        var startingPos = (0, 0);

        var rawMap = input[0].Split("\n").Select(line => line.ToCharArray()).ToArray();
        var map = new Tile[rawMap.Length][];
        for (var y = 0; y < rawMap.Length; y++)
        {
            var tilesLine = new Tile[rawMap.Length];
            for (var x = 0; x < tilesLine.Length; x++)
            {
                switch (rawMap[y][x])
                {
                    case '#':
                        tilesLine[x] = Tile.Wall;
                        break;
                    case '.':
                        tilesLine[x] = Tile.Empty;
                        break;
                    case 'O':
                        tilesLine[x] = Tile.Box;
                        break;
                    case '@':
                        tilesLine[x] = Tile.Empty;
                        startingPos = (x, y);
                        break;
                }
            }

            map[y] = tilesLine;
        }

        var directionsRaw = input[1].Replace("\n", "");
        var movements = new Direction[directionsRaw.Length];
        for (var i = 0; i < directionsRaw.Length; i++)
        {
            switch (directionsRaw[i])
            {
                case '^':
                    movements[i] = Direction.Up;
                    break;
                case '>':
                    movements[i] = Direction.Right;
                    break;
                case 'v':
                    movements[i] = Direction.Down;
                    break;
                case '<':
                    movements[i] = Direction.Left;
                    break;
            }
        }

        return new ParsedInput(map, startingPos, movements);
    }

    // For debugging
    internal static void PrintMap(Tile[][] map, (int, int) currPosition)
    {
        for (var y = 0; y < map.Length; y++)
        {
            for (var x = 0; x < map[0].Length; x++)
            {
                switch (map[y][x])
                {
                    case Tile.Wall:
                        Console.Write("#");
                        break;
                    case Tile.Empty:
                        if ((x, y) == currPosition)
                        {
                            Console.Write("@");
                        }
                        else
                        {
                            Console.Write(".");
                        }
                        break;
                    case Tile.Box:
                        Console.Write("0");
                        break;
                }
            }
            Console.WriteLine();
        }
    }

    internal static (int, int) MoveRobot(Tile[][] map, (int, int) position, Direction direction)
    {
        var newPosition = direction.Move(position);
        var newPositionTile = map[newPosition.Item2][newPosition.Item1];
        switch (newPositionTile)
        {
            case Tile.Wall:
                return position;
            case Tile.Empty:
                return newPosition;
            case Tile.Box:
                if (TryToMoveBox(map, Tile.Empty, newPosition, direction))
                {
                    return newPosition;
                }
                else
                {
                    return position;
                }
            default: throw new UnreachableException("Invalid tile");
        }
    }

    internal static bool TryToMoveBox(Tile[][] map, Tile pushing, (int, int) position, Direction direction)
    {
        var newPosition = direction.Move(position);
        var newPositionTile = map[newPosition.Item2][newPosition.Item1];
        switch (newPositionTile)
        {
            case Tile.Wall:
                return false;
            case Tile.Empty:
                map[position.Item2][position.Item1] = pushing;
                map[newPosition.Item2][newPosition.Item1] = Tile.Box;

                return true;
            case Tile.Box:
                if (TryToMoveBox(map, Tile.Box, newPosition, direction))
                {
                    map[position.Item2][position.Item1] = pushing;
                    map[newPosition.Item2][newPosition.Item1] = Tile.Box;

                    return true;
                }
                else
                {
                    return false;
                }
            default: throw new UnreachableException("Invalid tile");
        }
    }

    internal static int GpsSum(Tile[][] map)
    {
        var total = 0;
        for (var y = 0; y < map.Length; y++)
        {
            for (var x = 0; x < map[0].Length; x++)
            {
                if (map[y][x] == Tile.Box)
                {
                    total += x + (y * 100);
                }
            }
        }

        return total;
    }

    internal readonly record struct ParsedInput(Tile[][] Map, (int, int) StartingPos, Direction[] Movements);

    internal enum Tile
    {
        Empty,
        Wall,
        Box,
    }
}
