#include "../base.cpp"

std::vector<int> parse_input(std::string input);

void part_one(std::string file_path)
{
    auto input = read_input(file_path);
    auto arr = parse_input(input);

    size_t curr = 0;
    size_t steps = 0;
    while (true)
    {
        steps++;
        int jumps = arr[curr];
        size_t next = curr + jumps;

        arr[curr]++;

        if (next >= arr.size() || next < 0)
        {
            break;
        }

        curr = next;
        continue;
    }

    std::cout << "Part One: " << steps << std::endl;
}

void part_two(std::string file_path)
{
}

std::vector<int> parse_input(std::string input) {
    std::vector<int> vec;

    for (auto item : split(input, '\n')) {
        vec.push_back(std::stoi(item));
    }

    return vec;
}

// --------------------------------------------------------------------------------------------------------------------

int main()
{
    auto input_path = "./input.txt";
    part_one(input_path);
    part_two(input_path);

    return 0;
}
