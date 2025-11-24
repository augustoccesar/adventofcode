using System.Reflection;

namespace aoc;

public abstract class Day
{
    public abstract string PartOne();
    public abstract string PartTwo();

    public int GetYear()
    {
        var attribute = this.GetType().GetCustomAttribute<RunnableDayAttribute>();
        return attribute?.Year ?? 0;
    }

    public int GetDay()
    {
        var attribute = this.GetType().GetCustomAttribute<RunnableDayAttribute>();
        return attribute?.Day ?? 0;
    }

    protected string[] ReadInputLines(string? name = null)
    {
        var fileName = GetFileName(name);
        var path = $"../inputs/{fileName}";

        if (!File.Exists(path))
        {
            throw new FileNotFoundException($"Input file not found: {path}");
        }

        return File.ReadAllLines(path);
    }

    protected string ReadInput(string? name = null)
    {
        return string.Join("", ReadInputLines(name));
    }

    private string GetFileName(string? name)
    {
        var year = GetYear();
        var day = GetDay();
        var dayPadded = day.ToString("D2");

        return name == null
            ? $"{year}_{dayPadded}.txt"
            : $"{year}_{dayPadded}_{name}.txt";
    }
}
