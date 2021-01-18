#include "../String.h"
#include "../Task.h"

class Day05 : public AbstractTask {
 public:
  std::string part_one() override {
    auto arr = parse_input(input());

    size_t curr = 0;
    size_t steps = 0;
    while (true) {
      steps++;
      int jumps = arr[curr];
      size_t next = curr + jumps;

      arr[curr]++;

      if (next >= arr.size()) {
        break;
      }

      curr = next;
    }

    return std::to_string(steps);
  }

  std::string part_two() override {
    auto arr = parse_input(input());

    size_t curr = 0;
    size_t steps = 0;
    while (true) {
      steps++;
      int offset = arr[curr];
      size_t next = curr + offset;

      if (offset >= 3) {
        arr[curr]--;
      } else {
        arr[curr]++;
      }

      if (next >= arr.size()) {
        break;
      }

      curr = next;
    }

    return std::to_string(steps);
  }

 private:
  std::string input() { return read_input("inputs/day05_input.txt"); }

  static std::vector<int> parse_input(const std::string& input) {
    std::vector<std::string> lines = split(input, '\n');

    std::vector<int> vec;
    std::transform(
        lines.begin(), lines.end(), std::back_inserter(vec),
        [](const std::string& line) -> int { return std::stoi(line); });

    return vec;
  }
};
