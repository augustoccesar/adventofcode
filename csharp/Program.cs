using System.ComponentModel.Design;
using aoc;
using ConsoleAppFramework;

static void RunCommand(int year, int day)
{
    var dayRegistry = DayRegistry.Load();
    var dayInstance = dayRegistry.GetDay(year, day);

    if (dayInstance == null)
    {
        Console.WriteLine($"Day {day} not found in year {year}");
        Environment.Exit(1);
    }

    Console.WriteLine(dayInstance.PartOne());
    Console.WriteLine(dayInstance.PartTwo());
}

static void DaysCommand()
{
    var dayRegistry = DayRegistry.Load();

    var yearDaysIter = from key in dayRegistry.DaysMap.Keys.Select(key => key.Split("-"))
                       group key[1] by key[0] into yearDays
                       select new { Year = yearDays.Key, Days = yearDays.ToList() };

    foreach (var yearDays in yearDaysIter)
    {
        Console.WriteLine($"{yearDays.Year};{string.Join(";", yearDays.Days)}");
    }
}

var app = ConsoleApp.Create();

app.Add("run", RunCommand);
app.Add("days", DaysCommand);

app.Run(args);
