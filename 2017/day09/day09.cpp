#include "../Task.h"

class Day09 : public AbstractTask {
 public:
  std::string part_one() override {
    auto input = read_input("inputs/day09_input.txt");

    int groupDepth = 0;
    int groupScore = 0;
    bool inGarbage = false;
    for (int i = 0; i < input.size(); i++) {
      if (input[i] == '!') {
        i += 1;
        continue;
      }

      if (input[i] == '<') {
        inGarbage = true;
        continue;
      }

      if (input[i] == '>') {
        inGarbage = false;
        continue;
      }

      if (input[i] == '{' && !inGarbage) {
        groupDepth++;
        continue;
      }

      if (input[i] == '}' && !inGarbage) {
        groupScore += groupDepth;
        groupDepth--;
      }
    }

    return std::to_string(groupScore);
  }

  std::string part_two() override { return "-"; }
};
