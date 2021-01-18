#include <math.h>

#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <vector>

#include "../Task.h"

using point = std::tuple<int, int>;

class Day03 : public AbstractTask {
 public:
  std::string part_one() override {
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
    // (-1, 1)  (0, 1)  (1, 1)
    // (-1, 0)  (0, 0)  (1, 0)
    // (-1, -1) (0, -1) (1, -1)

    // 1, 9, 25, 49 are perfect squares of odd numbers
    int square_id = std::stoi(input());
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

  std::string part_two() override {
    // Visualization
    // 147  142  133  122   59
    // 304    5    4    2   57
    // 330   10    1    1   54
    // 351   11   23   25   26
    // 362  747  806  1587 2467 ...

    // (-2, 2)  (-1, 2)  (0, 2)  (1, 2)  (2, 2)
    // (-2, 1)  (-1, 1)  (0, 1)  (1, 1)  (2, 1)
    // (-2, 0)  (-1, 0)  (0, 0)  (1, 0)  (2, 0)
    // (-2, -1) (-1, -1) (0, -1) (1, -1) (2, -1)
    // (-2, -2) (-1, -2) (0, -2) (1, -2) (2, -2) ...
    int looking_for = std::stoi(input());
    int result = -1;

    std::map<std::string, int> hist;
    point curr_point = {1, 0};
    hist[p_key({0, 0})] = 1;

    // Which direction currently moving
    // +y = 0
    // -x = 1
    // -y = 2
    // +x = 3
    int moving = 0;

    point next_exit_corner = {1, -1};

    while (true) {
      point next_point;
      auto [curr_x, curr_y] = curr_point;

      auto neighbors = neighbors_from_hist(curr_point, hist);

      int sum = std::accumulate(neighbors.begin(), neighbors.end(), 0,
                                [](int sum, const std::pair<point, int>& curr) {
                                  return sum + curr.second;
                                });

      if (sum > looking_for) {
        result = sum;
        break;
      }

      hist[p_key(curr_point)] = sum;

      if (abs(curr_x) == abs(curr_y)) {  // Is corner
        if (moving == 3) {
          moving = 0;
        } else {
          moving++;
        }
      }

      if (curr_point == next_exit_corner) {
        next_point = {curr_x + 1, curr_y};
        auto [nec_x, nec_y] = next_exit_corner;
        next_exit_corner = {nec_x + 1, nec_y - 1};
      } else {
        switch (moving) {
          case 0:
            next_point = {curr_x, curr_y + 1};
            break;
          case 1:
            next_point = {curr_x - 1, curr_y};
            break;
          case 2:
            next_point = {curr_x, curr_y - 1};
            break;
          case 3:
            next_point = {curr_x + 1, curr_y};
            break;
          default:
            throw "Invalid moving";
        }
      }

      curr_point = next_point;
    }

    return std::to_string(result);
  }

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

  std::string p_key(point p) {
    return std::to_string(std::get<0>(p)) + "," +
           std::to_string(std::get<1>(p));
  }

  std::vector<std::pair<point, int>> neighbors_from_hist(
      point p, std::map<std::string, int> hist) {
    std::vector<std::pair<point, int>> list;
    std::vector<std::pair<int, int>> modifiers{
        std::pair(-1, 1), std::pair(0, 1),   std::pair(1, 1),  std::pair(-1, 0),
        std::pair(1, 0),  std::pair(-1, -1), std::pair(0, -1), std::pair(1, -1),
    };

    for (auto& mod : modifiers) {
      auto [mod_x, mod_y] = mod;
      auto [x, y] = p;
      point new_point = {x + mod_x, y + mod_y};

      auto found = hist.find(p_key(new_point));
      if (found != hist.end()) {
        list.push_back(std::pair(new_point, found->second));
      }
    }

    return list;
  }
};
