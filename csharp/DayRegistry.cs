using System.Reflection;

namespace aoc;

public class DayRegistry
{
    private readonly Dictionary<string, Day> _dayMap = new();

    public static DayRegistry Load()
    {
        var registry = new DayRegistry();
        var assembly = Assembly.GetExecutingAssembly();

        var dayTypes = assembly.GetTypes()
            .Where(type => type.IsSubclassOf(typeof(Day)) &&
                          type.GetCustomAttribute<RunnableDayAttribute>() != null);

        foreach (var dayType in dayTypes)
        {
            var dayInstance = (Day)Activator.CreateInstance(dayType)!;
            var year = dayInstance.GetYear();
            var day = dayInstance.GetDay();
            var key = DayKey(year, day);

            if (registry._dayMap.ContainsKey(key))
            {
                throw new InvalidOperationException(
                    $"Day {day} for year {year} is defined more than once");
            }

            registry._dayMap[key] = dayInstance;
        }

        return registry;
    }

    public Day? GetDay(int year, int day)
    {
        var key = DayKey(year, day);
        return _dayMap.TryGetValue(key, out var dayInstance) ? dayInstance : null;
    }

    private static string DayKey(int year, int day) => $"{year}-{day}";
}
