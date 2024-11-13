using System.Reflection;
using System.Text;

abstract class Task
{
    protected string Input { get; }

    protected Task()
    {
        Input = LoadInput();
    }

    public abstract string PartOne();
    public abstract string PartTwo();

    private string LoadInput()
    {
        var info = Assembly.GetExecutingAssembly().GetName();
        var name = info.Name;
        var inputFileName = $"_{name}.inputs.{GetType().Name.ToLower()}_input.txt";

        using var stream = Assembly
                    .GetExecutingAssembly()
                    .GetManifestResourceStream(inputFileName)!;

        using var streamReader = new StreamReader(stream, Encoding.UTF8);

        return streamReader.ReadToEnd();
    }
}
