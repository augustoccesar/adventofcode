using System.Text.RegularExpressions;

partial class Day03 : Task
{
    [GeneratedRegex(@"mul\((\d{1,3}),(\d{1,3})\)")]
    private partial Regex MulPattern();

    public override string PartOne(string fileName)
    {
        var program = Input.ReadString(fileName);

        var total = 0;
        foreach (Match match in MulPattern().Matches(program))
        {
            total += int.Parse(match.Groups[1].ToString()) * int.Parse(match.Groups[2].ToString());
        }

        return total.ToString();
    }

    public override string PartTwo(string fileName)
    {
        var program = Input.ReadString(fileName);

        var enabled = true;

        var total = 0;
        for (int i = 0; i < program.Length; i++)
        {
            // instruction      | length
            // ---------------- | ------
            // mul(1, 2)        | 9
            // mul(10, 20)      | 11
            // mul(100, 200)    | 13
            // do()             | 4
            // don't()          | 7

            var c = program[i];
            if (c == 'm')
            {
                var maxIdx = Math.Min(i + 13, program.Length - 1);
                var subString = program[i..maxIdx];

                if (MulPattern().Match(subString) is var match && match.Length > 0)
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
