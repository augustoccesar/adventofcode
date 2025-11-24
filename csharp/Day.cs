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

    protected string[] ReadInputLines(string? suffix = null)
    {
        return File.ReadAllLines(GetFilePath(suffix));
    }

    protected string ReadInput(string? suffix = null)
    {
        return File.ReadAllText(GetFilePath(suffix));
    }

    private string GetFilePath(string? suffix = null)
    {
        var fileName = GetFileName(suffix);
        var path = $"../inputs/{fileName}";

        if (!File.Exists(path))
        {
            throw new FileNotFoundException($"Input file not found: {path}");
        }

        return path;
    }

    private string GetFileName(string? suffix)
    {
        var year = GetYear();
        var day = GetDay();
        var dayPadded = day.ToString("D2");

        return suffix == null
            ? $"{year}_{dayPadded}.txt"
            : $"{year}_{dayPadded}_{suffix}.txt";
    }
}
