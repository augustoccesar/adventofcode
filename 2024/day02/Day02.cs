class Day02 : Task
{
    public override string PartOne(string fileName)
    {
        var reports = Input.ReadLines(fileName).Select(line => line.Split(" ").ToList().ConvertAll(int.Parse));

        var safeReports = 0;
        foreach (var report in reports)
        {
            if (IsSafeReport(report))
            {
                safeReports++;
            }
        }

        return safeReports.ToString();
    }

    public override string PartTwo(string fileName)
    {
        var reports = Input.ReadLines(fileName).Select(line => line.Split(" ").ToList().ConvertAll(int.Parse));

        var safeReports = 0;
        foreach (var report in reports)
        {
            var safe = IsSafeReport(report);
            if (safe)
            {
                safeReports++;
                continue;
            }

            var i = 0;
            while (!safe && i < report.Count)
            {
                var newReport = new List<int>(report);
                newReport.RemoveAt(i);
                safe = IsSafeReport(newReport);
                i++;
            }

            if (safe)
            {
                safeReports++;
            }
        }

        return safeReports.ToString();
    }

    private static bool IsSafeReport(List<int> report)
    {
        var direction = 0;

        var safe = true;
        for (var i = 1; i < report.Count; i++)
        {
            var prev = report[i - 1];
            var curr = report[i];

            if (curr == prev)
            {
                safe = false;
                break;
            };

            var currDirection = curr > prev ? 1 : -1;

            if (direction == 0)
            {
                direction = currDirection;
            }

            if (direction != currDirection)
            {
                safe = false;
                break;
            }
            else if (Math.Abs(curr - prev) > 3)
            {
                safe = false;
                break;
            }
        }

        return safe;
    }
}
