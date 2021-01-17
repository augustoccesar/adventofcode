#include <fstream>

#include "../String.h"
#include "../Task.h"

class Day02 : public AbstractTask {
 public:
  std::string part_one() override {
    auto infile = input();

    int checksum = 0;
    while (!infile.eof()) {
      std::string line;
      getline(infile, line);

      std::vector<int> lineItems;
      for (const auto& itemStr : split(line, ' ')) {
        lineItems.push_back(std::stoi(itemStr));
      }

      auto max = *std::max_element(lineItems.begin(), lineItems.end());
      auto min = *std::min_element(lineItems.begin(), lineItems.end());
      checksum += max - min;
    }

    return std::to_string(checksum);
  }

  std::string part_two() override {
    auto infile = input();

    int checksum = 0;
    while (!infile.eof()) {
      std::string line;
      getline(infile, line);

      std::vector<int> lineItems;
      for (const auto& itemStr : split(line, ' ')) {
        auto item = std::stoi(itemStr);
        lineItems.push_back(item);
      }

      bool found = false;
      for (size_t i = 0; i < lineItems.size() && !found; i++) {
        for (size_t j = i + 1; j < lineItems.size() && !found; j++) {
          int base = std::max(lineItems[i], lineItems[j]);
          int divider = std::min(lineItems[j], lineItems[i]);

          if (base % divider == 0) {
            checksum += base / divider;
            found = true;
          }
        }
      }
    }

    return std::to_string(checksum);
  }

 private:
  static std::ifstream input() {
    std::ifstream infile("inputs/day02_input.txt");
    return infile;
  }
};
