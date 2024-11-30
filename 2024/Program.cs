using System.Reflection;

class Program
{
    public static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            throw new ArgumentException("Invalid number of arguments.");
        }

        string dayArg = args[0];
        string inputName = "input";
        for (var i = 1; i < args.Length; i++)
        {
            if (args[i] == "--input")
            {
                inputName = args[i + 1];
                i += 1;
            }
        }

        var day = int.Parse(dayArg).ToString().PadLeft(2, '0');
        var taskName = $"Day{day}";

        var task = FindTask(taskName);
        if (task == null)
        {
            Console.WriteLine($"Day {day} not found.");
            Environment.Exit(1);
        }

        var inputStream = Input.GetStream(day, inputName);
        if (inputStream == null)
        {
            Console.WriteLine($"Input '{inputName}' not found.");
            Environment.Exit(1);
        }

        var partOne = task.PartOne(inputStream);
        Console.WriteLine($"Part One: {partOne}");

        var partTwo = task.PartTwo(inputStream);
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
