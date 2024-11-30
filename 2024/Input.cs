using System.Reflection;

class Input
{
    public static string? GetFileName(string day, string inputName)
    {
        var info = Assembly.GetExecutingAssembly().GetName();
        var name = info.Name;
        var fileName = $"_{name}.inputs.day{day}_{inputName}.txt";

        var fileInfo = Assembly.GetExecutingAssembly().GetManifestResourceInfo(fileName);
        if (fileInfo == null)
        {
            return null;
        }

        return fileName;
    }

    public static string ReadString(string fileName)
    {
        using var stream = Assembly
                        .GetExecutingAssembly()
                        .GetManifestResourceStream(fileName)!;

        byte[] buffer = new byte[stream.Length];
        int bytesRead = stream.Read(buffer, 0, (int)stream.Length);

        return System.Text.Encoding.UTF8.GetString(buffer, 0, bytesRead);
    }

    public static string[] ReadLines(string fileName)
    {
        var input = ReadString(fileName);
        return input.Split("\n");
    }

    public static List<T> ReadLinesAs<T>(string fileName, Converter<string, T> converter)
    {
        return ReadLines(fileName).ToList().ConvertAll(converter);
    }
}
