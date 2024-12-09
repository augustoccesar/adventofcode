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
        var fileMap = new Dictionary<int, Stack<int>>();

        var disk = new int?[diskmap.Sum()];
        for (int i = 0, diskIdx = 0; i < diskmap.Count; i++)
        {
            if (i % 2 == 0)
            {
                if (!fileMap.ContainsKey(diskmap[i])) fileMap.Add(diskmap[i], new Stack<int>());

                fileMap[diskmap[i]].Push(diskIdx);
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

        for (int i = 0; i < disk.Length;)
        {
            if (disk[i] == null)
            {
                var startIdx = i;
                var size = 0;
                while (disk[i] == null)
                {
                    size += 1;
                    i++;
                }

                var idxToMove = fileMap[size].Pop();

                for (int j = startIdx; j < (startIdx + size); j++) disk[j] = disk[idxToMove];
                for (int j = idxToMove; j < (idxToMove + size); j++) disk[j] = null;
            }
            else
            {
                i++;
            }
        }

        return "-";
    }
}
