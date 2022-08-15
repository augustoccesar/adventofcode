#include "vector"

#include "../String.h"
#include "../Task.h"

class Day10 : public AbstractTask {
 public:
  std::string part_one() override {
    int circleSize = 256;
    std::vector<int> lengths = parseInput(read_input("inputs/day10_input.txt"));

    int circle[circleSize];
    for (int i = 0; i < circleSize; i++) {
      circle[i] = i;
    }

    int position = 0;
    int skipSize = 0;
    for (auto length : lengths) {
      int pivotL = position;
      int pivotR = (position + length - 1);
      while (pivotL < pivotR) {
        int tmp = circle[pivotL % circleSize];
        circle[pivotL % circleSize] = circle[pivotR % circleSize];
        circle[pivotR % circleSize] = tmp;

        pivotL++;
        pivotR--;
      }

      position = (position + length + skipSize) % circleSize;
      skipSize++;
    }

    return std::to_string(circle[0] * circle[1]);
  }

  std::string part_two() override { return "-"; }

 private:
  std::vector<int> parseInput(const std::string& input) {
    std::vector<int> lengths;

    for (auto lengthStr : split(input, ',')) {
      lengths.push_back(std::stoi(lengthStr));
    }

    return lengths;
  }
};
