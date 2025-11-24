namespace aoc.y2024;

[RunnableDay(year: 2024, day: 5)]
class Day05 : Day
{
    public override string PartOne()
    {
        Dictionary<int, List<int>> rules = [];
        List<List<int>> updates = [];

        var parsingRules = true;
        foreach (var line in ReadInputLines())
        {
            if (line == "")
            {
                parsingRules = false;
                continue;
            }

            if (parsingRules)
            {
                var pages = line.Split("|").ToList().ConvertAll(int.Parse);
                if (rules.TryGetValue(pages[0], out var afterList))
                {
                    afterList.Add(pages[1]);
                }
                else
                {
                    rules[pages[0]] = [pages[1]];
                }
            }
            else
            {
                updates.Add(line.Split(",").ToList().ConvertAll(int.Parse));
            }
        }

        var result = 0;
        foreach (var update in updates)
        {
            var isSorted = true;
            for (var i = 1; i < update.Count && isSorted; i++)
            {
                if (rules.TryGetValue(update[i], out var smallerThan))
                {
                    for (var j = i - 1; j >= 0; j--)
                    {
                        if (smallerThan.Contains(update[j]))
                        {
                            isSorted = false;
                            break;
                        }
                    }
                }
            }

            if (isSorted)
            {
                result += update[update.Count / 2];
            }
        }

        return result.ToString();
    }

    public override string PartTwo()
    {
        Dictionary<int, List<int>> rules = [];
        List<List<int>> updates = [];

        var parsingRules = true;
        foreach (var line in ReadInputLines())
        {
            if (line == "")
            {
                parsingRules = false;
                continue;
            }

            if (parsingRules)
            {
                var pages = line.Split("|").ToList().ConvertAll(int.Parse);
                if (rules.TryGetValue(pages[0], out var afterList))
                {
                    afterList.Add(pages[1]);
                }
                else
                {
                    rules[pages[0]] = [pages[1]];
                }
            }
            else
            {
                updates.Add(line.Split(",").ToList().ConvertAll(int.Parse));
            }
        }


        var result = 0;
        foreach (var update in updates)
        {
            var isSorted = true;
            for (var i = 1; i < update.Count && isSorted; i++)
            {
                if (rules.TryGetValue(update[i], out var smallerThan))
                {
                    for (var j = i - 1; j >= 0; j--)
                    {
                        if (smallerThan.Contains(update[j]))
                        {
                            isSorted = false;
                            break;
                        }
                    }
                }
            }

            if (!isSorted)
            {
                var updateSorted = update.OrderBy(i => i, new UpdateComparer(rules)).ToList();
                result += updateSorted[updateSorted.Count / 2];
            }
        }

        return result.ToString();
    }
}

class UpdateComparer : IComparer<int>
{
    private Dictionary<int, List<int>> rules;

    public UpdateComparer(Dictionary<int, List<int>> rules)
    {
        this.rules = rules;
    }

    public int Compare(int x, int y)
    {
        if (rules.TryGetValue(x, out var smallerThan))
        {
            if (smallerThan.Contains(y))
            {
                return -1;
            }
        }

        return 1;
    }
}
