#include "../String.h"
#include "../Task.h"

typedef std::map<std::string, bool> History;

class Day06 : public AbstractTask {
 public:
  std::string part_one() override {
    std::cout << typeid(this).name() << std::endl;

    History history;
    std::vector<int> banks = buildBanksFromInput();

    int loops = 1;
    while (true) {
      unsigned int startIdx = 0;
      int largestValue = banks[0];  // amount to distribute
      for (int i = 0; i < banks.size(); i++) {
        if (banks[i] > largestValue) {
          largestValue = banks[i];
          startIdx = i;
        }
      }
      banks[startIdx] = 0;

      while (largestValue > 0) {
        startIdx = (startIdx + 1) % banks.size();
        banks[startIdx] += 1;
        largestValue--;
      }

      std::string print = fingerprint(banks);

      if (history.count(print)) {
        break;
      }

      history.insert(std::pair<std::string, bool>(print, true));
      loops++;
    }

    return std::to_string(loops);
  }

  std::string part_two() override {
    History history;
    std::vector<int> banks = buildBanksFromInput();

    std::string markFingerprint;
    int countSinceMarkFingerprint = 0;
    while (true) {
      if (!markFingerprint.empty()) {
        countSinceMarkFingerprint++;
      }

      unsigned int startIdx = 0;
      int largestValue = banks[0];  // amount to distribute
      for (int i = 0; i < banks.size(); i++) {
        if (banks[i] > largestValue) {
          largestValue = banks[i];
          startIdx = i;
        }
      }
      banks[startIdx] = 0;

      while (largestValue > 0) {
        startIdx = (startIdx + 1) % banks.size();
        banks[startIdx] += 1;
        largestValue--;
      }

      std::string print = fingerprint(banks);

      if (history.count(print)) {
        if (markFingerprint.empty()) {
          markFingerprint = print;
          continue;
        }

        if (print == markFingerprint) {
          break;
        }
      }

      history.insert(std::pair<std::string, bool>(print, true));
    }

    return std::to_string(countSinceMarkFingerprint);
  }

 private:
  static std::string input() { return read_input("inputs/day06_input.txt"); }

  static std::vector<int> buildBanksFromInput() {
    std::vector<std::string> splitInput = split(input(), '\t');
    std::vector<int> banks(splitInput.size(), -1);

    for (int i = 0; i < banks.size(); i++) {
      banks[i] = std::stoi(splitInput[i]);
    }

    return banks;
  }

  static std::string fingerprint(std::vector<int> arr) {
    std::string res;
    for (int i = 0; i < arr.size(); i++) {
      res += std::to_string(arr[i]);
      if (i < arr.size() - 1) {
        res += ",";
      }
    }

    return res;
  }
};
