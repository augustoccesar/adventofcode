class Day09 : Task
{
    public override string PartOne(string fileName)
    {
        var disk = BuildDisk(fileName);

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
        var disk = BuildDisk(fileName);

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

        var total = 0L;
        for (int i = 0; i < disk.Length; i++)
        {
            total += i * (disk[i] ?? 0);
        }

        return total.ToString();
    }

    private static Disk BuildDisk(string inputName)
    {
        var diskMap = Input.ReadString(inputName).AsEnumerable().Select(c => c - '0').ToList();
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

class Disk
{
    public int?[] Data { get; }

    public List<File> Files { get; }

    public int Length
    {
        get => Data.Length;
    }

    public Disk(int?[] Data, List<File> Files)
    {
        this.Data = Data;
        this.Files = Files;
    }

    public int? this[int i]
    {
        get => Data[i];
        set => Data[i] = value;
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

    public class File
    {
        public int ID { get; }
        public int DiskPosition { get; private set; }
        public int Size { get; }

        public File(int ID, int DiskPosition, int Size)
        {
            this.ID = ID;
            this.DiskPosition = DiskPosition;
            this.Size = Size;
        }

        public void MoveTo(int idx)
        {
            this.DiskPosition = idx;
        }
    }
};
