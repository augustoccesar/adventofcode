#include <algorithm>

#include "../String.h"
#include "../Task.h"

class Day11 : public AbstractTask {
 public:
  std::string part_one() override {
    std::string input = read_input("inputs/day11_input.txt");
    std::vector<std::string> directions = split(input, ',');
    std::tuple<int, int> starting_pos = {0, 0};

    std::tuple<int, int> current_pos = {0, 0};
    for (int i = 0; i < directions.size(); i++) {
      std::tuple<int, int> modifier = direction_modifier(directions[i]);

      current_pos = {std::get<0>(current_pos) + std::get<0>(modifier),
                     std::get<1>(current_pos) + std::get<1>(modifier)};
    }

    int res = distance(starting_pos, current_pos);

    return std::to_string(res);
  }

  std::string part_two() override { return "-"; }

  const std::tuple<int, int> direction_modifier(std::string& direction) {
    if (direction == "n") {
      return {0, 1};
    } else if (direction == "ne") {
      return {1, 0};
    } else if (direction == "se") {
      return {1, -1};
    } else if (direction == "s") {
      return {0, -1};
    } else if (direction == "sw") {
      return {-1, 0};
    } else if (direction == "nw") {
      return {-1, 1};
    } else {
      throw "invalid direction";
    }
  }

  int distance(std::tuple<int, int>& from, std::tuple<int, int>& to) {
    int diff_x = std::get<0>(from) - std::get<0>(to);
    int diff_y = std::get<1>(from) - std::get<1>(to);

    if (signbit(diff_x) == signbit(diff_y)) {
      return abs(diff_x + diff_y);
    } else {
      return std::max(abs(diff_x), abs(diff_y));
    }
  }
};
