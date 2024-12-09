class Day09 : Task
{
    public override string PartOne(string fileName)
    {
        var diskmap = Input.ReadString(fileName).AsEnumerable().Select(c => c - '0').ToList();
        var disk = new int?[diskmap.Sum()];
        for (int i = 0, diskIdx = 0; i < diskmap.Count; i++)
        {
            for (int j = 0; j < diskmap[i]; j++, diskIdx++)
            {
                if (i % 2 == 0)
                {
                    disk[diskIdx] = i / 2;
                }
                else
                {
                    disk[diskIdx] = null;
                }
            }
        }

        for (int i = 0, j = disk.Length - 1; i < disk.Length && i < j; i++)
        {
            if (disk[i] == null)
            {
                while (disk[j] == null) j--;

                disk[i] = disk[j];
                disk[j] = null;
                j--;
            }
        }

        var total = 0L;
        for (int i = 0; i < disk.Length; i++)
        {
            total += i * (disk[i] ?? 0);
        }

        return total.ToString();
    }

    public override string PartTwo(string fileName)
    {
        var diskmap = Input.ReadString(fileName).AsEnumerable().Select(c => c - '0').ToList();
        var filesSizes = new List<(int, int)>();

        var disk = new int?[diskmap.Sum()];
        for (int i = 0, diskIdx = 0; i < diskmap.Count; i++)
        {
            if (i % 2 == 0)
            {
                filesSizes.Add((diskmap[i], diskIdx));
            }

            for (int j = 0; j < diskmap[i]; j++, diskIdx++)
            {
                if (i % 2 == 0)
                {
                    disk[diskIdx] = i / 2;
                }
                else
                {
                    disk[diskIdx] = null;
                }
            }
        }

        for (int i = filesSizes.Count - 1; i >= 0; i--)
        {
            var lookupSize = filesSizes[i].Item1;
            var currentEmptySize = 0;
            var currentEmptyIdx = -1;
            for (int j = 0; j < disk.Length; j++)
            {
                if (disk[j] == null)
                {
                    if (currentEmptyIdx == -1) currentEmptyIdx = j;

                    currentEmptySize++;
                }
                else
                {
                    currentEmptyIdx = -1;
                    currentEmptySize = 0;
                }

                if (currentEmptySize == lookupSize && filesSizes[i].Item2 > currentEmptyIdx)
                {
                    for (int l = currentEmptyIdx; l < currentEmptyIdx + currentEmptySize; l++) disk[l] = disk[filesSizes[i].Item2];
                    for (int k = filesSizes[i].Item2; k < filesSizes[i].Item2 + currentEmptySize; k++) disk[k] = null;

                    currentEmptyIdx = -1;
                    currentEmptySize = 0;
                    break;
                }
            }
        }

        var total = 0L;
        for (int i = 0; i < disk.Length; i++)
        {
            total += i * (disk[i] ?? 0);
        }

        return total.ToString();
    }
}
