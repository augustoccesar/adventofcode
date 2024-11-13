using System.Reflection;

class Program
{
    public static void Main(string[] args)
    {
        if (args.Length != 1)
        {
            throw new ArgumentException("Invalid number of arguments.");
        }

        var day = int.Parse(args[0]).ToString().PadLeft(2, '0');
        var taskName = $"Day{day}";

        var task = FindTask(taskName);
        if (task == null)
        {
            Console.WriteLine($"Day {day} not found.");
            Environment.Exit(-1);
        }

        var partOne = task.PartOne();
        Console.WriteLine($"Part One: {partOne}");

        var partTwo = task.PartTwo();
        Console.WriteLine($"Part Two: {partTwo}");
    }

    private static Task? FindTask(string name)
    {
        var runnable = Assembly
                    .GetExecutingAssembly()
                    .GetTypes()
                    .Where(t => t.BaseType == typeof(Task))
                    .FirstOrDefault(task => task.Name == name);

        if (runnable == null)
        {
            return null;
        }

        return (Task)Activator.CreateInstance(runnable)!;
    }
}
