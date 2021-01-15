#include <math.h>

#include <algorithm>
#include <iostream>
#include <map>
#include <vector>

#include "../Task.h"

// Visualization
// 37  36  35  34  33  32  31
// 38  17  16  15  14  13  30
// 39  18   5   4   3  12  29
// 40  19   6   1   2  11  28
// 41  20   7   8   9  10  27
// 42  21  22  23  24  25  26
// 43  44  45  46  47  48  49...

// 5  4  3
// 6  1  2
// 7  8  9
//
// (-1, 1) (0, 1) (1, 1)
// (-1, 0) (0, 0) (1, 0)
// (-1, -1) (0, -1) (1, -1)

// 1, 9, 25, 49 are perfect squares of odd numbers

using point = std::tuple<int, int>;

class Day03 : public AbstractTask {
 public:
  std::string part_one() override {
    int square_id = std::stoi(read_input("inputs/day03_input.txt"));
    int next_sqrt = sqrt(square_id);

    if (next_sqrt % 2 == 0) {
      next_sqrt += 1;
    } else {
      next_sqrt += 2;
    }

    int closest_perfect_square = pow(next_sqrt, 2);
    std::tuple<int, int> closest_perfect_square_pos = {(next_sqrt - 1) / 2,
                                                       -((next_sqrt - 1) / 2)};

    auto line = square_layer_from_bottom_right(closest_perfect_square_pos);
    std::map<long, point> lookup;

    for (size_t i = 0; i < line.size(); i++) {
      lookup[closest_perfect_square - i] = line[i];
    }

    point square_pos = lookup[square_id];

    int dist = taxicab({0, 0}, square_pos);

    return std::to_string(dist);
  }

  std::string part_two() override { return ""; }

 private:
  std::string input() { return read_input("inputs/day03_input.txt"); }

  std::vector<point> line_points(point start, point end) {
    std::vector<point> line;

    bool swapped = false;
    auto [start_x, start_y] = start;
    auto [end_x, end_y] = end;

    if (start_x == end_x) {  // Is vertical
      if (end_y < start_y) {
        // Make start be the smallest one
        std::swap(start_y, end_y);
        swapped = true;
      }

      for (int y = start_y; y <= end_y; y++) {
        line.push_back({start_x, y});
      }
    } else if (start_y == end_y) {  // Is horizontal
      if (end_x < start_x) {
        // Make start be the smallest one
        std::swap(start_x, end_x);
        swapped = true;
      }

      for (int x = start_x; x <= end_x; x++) {
        line.push_back({x, start_y});
      }
    } else {
      // Is diagonal TODO: Should I even care about this one for my use?
      throw "Diagonal line";
    }

    if (swapped) {
      std::reverse(line.begin(), line.end());
    }

    return line;
  }

  std::vector<point> square_layer_from_bottom_right(point base) {
    auto [x, y] = base;

    auto line1 = line_points(base, {-x, y});      // Bottom line: invert x
    auto line2 = line_points({-x, y}, {-x, -y});  // Left line: invert both
    auto line3 = line_points({-x, -y}, {x, -y});  // Top line: invert y
    auto line4 = line_points({x, -y}, base);  // Right line: go back to start

    // Append the lines
    line1.insert(line1.end(), line2.begin(), line2.end());
    line1.insert(line1.end(), line3.begin(), line3.end());
    line1.insert(line1.end(), line4.begin(), line4.end());

    // Remove duplicates inside the line
    line1.erase(unique(line1.begin(), line1.end()), line1.end());

    // Remove last point (is the same as the start)
    line1.erase(line1.end() - 1);

    return line1;
  }

  int taxicab(point a, point b) {
    auto [a_x, a_y] = a;
    auto [b_x, b_y] = b;
    return abs(a_x - b_x) + abs(a_y - b_y);
  }
};
