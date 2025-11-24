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

var app = ConsoleApp.Create();

app.Add("run", RunCommand);

app.Run(args);
