namespace aoc.y2024;

[RunnableDay(year: 2024, day: 9)]
class Day09 : Day
{
    public override string PartOne()
    {
        var map = ReadInput().AsEnumerable().Select(c => c - '0').ToList();
        var disk = BuildDisk(map);

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

        return disk.Checksum().ToString();
    }

    public override string PartTwo()
    {
        var map = ReadInput().AsEnumerable().Select(c => c - '0').ToList();
        var disk = BuildDisk(map);

        for (int i = disk.Files.Count - 1; i >= 0; i--)
        {
            var lookupSize = disk.Files[i].Size;
            var currentEmptySize = 0;
            var currentEmptyIdx = -1;
            for (int j = 0; j < disk.Files[i].DiskPosition; j++)
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

                if (currentEmptySize == lookupSize && disk.Files[i].DiskPosition > currentEmptyIdx)
                {
                    disk.MoveFile(disk.Files[i], currentEmptyIdx);
                    break;
                }
            }
        }

        return disk.Checksum().ToString();
    }

    private static Disk BuildDisk(List<int> diskMap)
    {
        var files = new List<Disk.File>();

        var diskData = new int?[diskMap.Sum()];
        for (int i = 0, diskIdx = 0; i < diskMap.Count; i++)
        {
            if (i % 2 == 0)
            {
                files.Add(new Disk.File(i / 2, diskIdx, diskMap[i]));
            }

            for (int j = 0; j < diskMap[i]; j++, diskIdx++)
            {
                if (i % 2 == 0)
                {
                    diskData[diskIdx] = i / 2;
                }
                else
                {
                    diskData[diskIdx] = null;
                }
            }
        }

        return new Disk(diskData, files);
    }
}

class Disk(int?[] Data, List<Disk.File> Files)
{
    public int?[] Data { get; } = Data;

    public List<File> Files { get; } = Files;

    public int Length
    {
        get => Data.Length;
    }

    public int? this[int i]
    {
        get => Data[i];
        set => Data[i] = value;
    }

    public long Checksum()
    {
        var total = 0L;
        for (int i = 0; i < Data.Length; i++)
        {
            total += i * (Data[i] ?? 0);
        }

        return total;
    }

    public void MoveFile(File file, int idx)
    {
        if (Data[idx..(idx + file.Size)].All(x => x == null))
        {
            for (int i = idx; i < idx + file.Size; i++) Data[i] = file.ID;
            for (int i = file.DiskPosition; i < file.DiskPosition + file.Size; i++) Data[i] = null;
            file.MoveTo(idx);
        }
        else throw new Exception("Invalid move");
    }

    public class File(int ID, int DiskPosition, int Size)
    {
        public int ID { get; } = ID;
        public int DiskPosition { get; private set; } = DiskPosition;
        public int Size { get; } = Size;

        public void MoveTo(int idx)
        {
            this.DiskPosition = idx;
        }
    }
};

