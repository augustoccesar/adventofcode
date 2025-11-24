namespace aoc;

[AttributeUsage(AttributeTargets.Class)]
public class RunnableDayAttribute(int year, int day) : Attribute
{
    public int Year { get; } = year;
    public int Day { get; } = day;
}
