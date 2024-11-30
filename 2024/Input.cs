using System.Reflection;

class Input
{
    public static Stream? GetStream(string day, string inputName)
    {
        var info = Assembly.GetExecutingAssembly().GetName();
        var name = info.Name;
        var inputFileName = $"_{name}.inputs.day{day}_{inputName}.txt";

        return Assembly
                    .GetExecutingAssembly()
                    .GetManifestResourceStream(inputFileName);
    }

    public static string ReadString(Stream fileStream)
    {
        byte[] buffer = new byte[fileStream.Length];
        int bytesRead = fileStream.Read(buffer, 0, (int)fileStream.Length);

        fileStream.Seek(0, SeekOrigin.Begin);

        return System.Text.Encoding.UTF8.GetString(buffer, 0, bytesRead);
    }

    public static string[] ReadLines(Stream fileStream)
    {
        var input = ReadString(fileStream);
        return input.Split("\n");
    }
}
