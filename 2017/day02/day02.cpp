#include "../base.cpp"

void part_one(std::string input_file)
{
    std::ifstream infile(input_file);

    int checksum = 0;
    while (!infile.eof())
    {
        std::string line;
        getline(infile, line);

        std::vector<int> lineItems;
        for (auto itemStr : split(line, ' '))
        {
            lineItems.push_back(std::stoi(itemStr));
        }

        auto max = *std::max_element(lineItems.begin(), lineItems.end());
        auto min = *std::min_element(lineItems.begin(), lineItems.end());
        checksum += max - min;
    }

    printf("Part One: %d\n", checksum);
}

void part_two(std::string input_file)
{
    std::ifstream infile(input_file);

    int checksum = 0;
    while (!infile.eof())
    {
        std::string line;
        getline(infile, line);

        std::vector<int> lineItems;
        for (auto itemStr : split(line, ' '))
        {
            auto item = std::stoi(itemStr);
            lineItems.push_back(item);
        }

        bool found = false;
        for (size_t i = 0; i < lineItems.size() && !found; i++)
        {
            for (size_t j = i + 1; j < lineItems.size() && !found; j++)
            {
                int base = std::max(lineItems[i], lineItems[j]);
                int divider = std::min(lineItems[j], lineItems[i]);

                if (base % divider == 0)
                {
                    checksum += base / divider;
                    found = true;
                }
            }
        }
    }

    printf("Part Two: %d\n", checksum);
}

int main()
{
    auto input_file = "./input.txt";
    part_one(input_file);
    part_two(input_file);
    return 0;
}
