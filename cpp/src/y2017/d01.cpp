#include "../Day.hpp"

class Day01 : public Day
{
public:
  Day01(int year, int day) : Day(year, day) {}

  std::string partOne() override
  {
    return std::to_string(calculate(input(), 1));
  }

  std::string partTwo() override
  {
    auto data = input();
    size_t steps = data.length() / 2;
    return std::to_string(calculate(data, steps));
  }

private:
  std::string input() { return readInput(""); }

  static int calculate(std::string input, int steps)
  {
    int sum = 0;
    for (size_t i = 0; i < input.length(); i++)
    {
      size_t next_idx = (i + steps) % input.length();
      if (input[i] == input[next_idx])
        sum += c_to_digit(input[i]);
    }

    return sum;
  }

  static int c_to_digit(char c) { return (c - '0'); }
};

REGISTER_DAY(2017, 1, Day01);
