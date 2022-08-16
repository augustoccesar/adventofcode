#include "vector"

#include "../String.h"
#include "../Task.h"

class Day10 : public AbstractTask {
 public:
  std::string part_one() override {
    std::vector<int> lengths = parseInput(read_input("inputs/day10_input.txt"));

    int circleSize = 256;
    int circle[circleSize];
    initCircle(circle, circleSize);

    int position = 0;
    int skipSize = 0;
    executeLenghts(lengths, circle, circleSize, position, skipSize);

    return std::to_string(circle[0] * circle[1]);
  }

  std::string part_two() override {
    std::vector<int> lengths = parseInputBytes(read_input("inputs/day10_input.txt"));

    int circleSize = 256;
    int circle[circleSize];
    initCircle(circle, circleSize);

    int position = 0;
    int skipSize = 0;
    for (int i = 0; i < 64; i++) {
      executeLenghts(lengths, circle, circleSize, position, skipSize);
    }

    int blocks[16];
    fillBlocks(blocks, circle);

    std::string res = "";
    for (auto block : blocks) {
      res += intToHex(block);
    }

    return res;
  }

 private:
  void initCircle(int circle[], int size) {
    for (int i = 0; i < size; i++) {
      circle[i] = i;
    }
  }

  void executeLenghts(std::vector<int> lengths, int circle[], int circleSize, int& position, int& skipSize) {
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
  }

  void fillBlocks(int blocks[], int circle[]) {
    for (int i = 0; i < 16; i++) {
      int blockStartIdx = i * 16;
      int blockEndIdx = blockStartIdx + 16;

      int block = 0;
      for(int j = blockStartIdx; j < blockEndIdx; j++){
        block ^= circle[j];        
      }

      blocks[i] = block;
    }
  }

  std::vector<int> parseInput(const std::string& input) {
    std::vector<int> lengths;

    for (auto lengthStr : split(input, ',')) {
      lengths.push_back(std::stoi(lengthStr));
    }

    return lengths;
  }

  std::vector<int> parseInputBytes(const std::string& input) {
    std::vector<int> lengths;
    int extraLengths[] = {17, 31, 73, 47, 23};

    for (int i = 0; i < input.size(); i++) {
      lengths.push_back(int(input[i]));
    }

    for (int el : extraLengths) {
      lengths.push_back(el);
    }

    return lengths;
  }

  std::string intToHex(int val) {
    std::ostringstream ss;
    ss << std::hex << val;
    auto hexVal = ss.str();
    if(hexVal.size() == 1) {
      return "0" + hexVal;
    }

    return hexVal;
  }
};
