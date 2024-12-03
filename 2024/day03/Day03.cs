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
        var program = Input.ReadString(fileName);
        var mulPattern = @"mul\((\d{1,3}),(\d{1,3})\)";

        var enabled = true;

        // mul(1, 2)        | 9
        // mul(10, 20)      | 11
        // mul(100, 200)    | 13
        // do()             | 4
        // don't()          | 7

        var total = 0;
        for (int i = 0; i < program.Length; i++)
        {
            var c = program[i];
            if (c == 'm')
            {
                var maxIdx = Math.Min(i + 13, program.Length - 1);
                var subString = program[i..maxIdx];
                if (Regex.Match(subString, mulPattern) is var match && match.Length > 0)
                {
                    i += match.Length - 1;

                    if (enabled)
                    {
                        total += int.Parse(match.Groups[1].ToString()) * int.Parse(match.Groups[2].ToString());
                    }
                }
            }
            else if (c == 'd')
            {
                var maxIdx = Math.Min(i + 7, program.Length - 1);
                var subString = program[i..maxIdx];
                if (subString.StartsWith("do()"))
                {
                    enabled = true;
                    i += 4 - 1;
                }
                else if (subString.StartsWith("don't()"))
                {
                    enabled = false;
                    i += 7 - 1;
                }
            }
        }


        return total.ToString();
    }
}
