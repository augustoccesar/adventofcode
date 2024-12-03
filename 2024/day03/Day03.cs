using System.Text.RegularExpressions;

class Day03 : Task
{
    public override string PartOne(string fileName)
    {
        var program = Input.ReadString(fileName);

        var total = 0;
        foreach (Match match in Regex.Matches(program, @"mul\((\d{1,3}),(\d{1,3})\)"))
        {
            total += int.Parse(match.Groups[1].ToString()) * int.Parse(match.Groups[2].ToString());
        }

        return total.ToString();
    }

    public override string PartTwo(string fileName)
    {
        return "-";
    }
}
