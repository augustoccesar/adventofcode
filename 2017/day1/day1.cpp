#include "../base.cpp"

using namespace std;

string input = read_input("./input.txt");

int calculate(string input, int steps)
{
    int sum = 0;
    for (size_t i = 0; i < input.length(); i++)
    {
        int next_idx = (i + steps) % input.length();
        if (input[i] == input[next_idx])
            sum += c_to_digit(input[i]);
    }

    return sum;
}

void part_one()
{
    printf("Part One: %d\n", calculate(input, 1));
}

void part_two()
{
    int steps = input.length() / 2;
    printf("Part Two: %d\n", calculate(input, steps));
}

int main()
{
    part_one();
    part_two();
    return 0;
}
