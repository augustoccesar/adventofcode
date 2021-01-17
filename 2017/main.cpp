#include <iostream>
#include <unordered_map>

#include "day01/day01.cpp"
#include "day02/day02.cpp"
#include "day03/day03.cpp"
#include "day04/day04.cpp"
#include "day05/day05.cpp"

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Invalid amount of arguments" << std::endl;
    exit(1);
  }

  auto i_day = std::stoi(argv[1]);
  AbstractTask *day;

  switch (i_day) {
    case 1: {
      auto day01 = Day01{};
      day = &day01;
      break;
    }
    case 2: {
      auto day02 = Day02{};
      day = &day02;
      break;
    }
    case 3: {
      auto day03 = Day03{};
      day = &day03;
      break;
    }
    case 4: {
      auto day04 = Day04{};
      day = &day04;
      break;
    }
    case 5: {
      auto day05 = Day05{};
      day = &day05;
      break;
    }
    default:
      std::cerr << "Day not found" << std::endl;
      exit(1);
  }

  std::cout << "Part One: " << day->part_one() << std::endl;
  std::cout << "Part Two: " << day->part_two() << std::endl;
  exit(0);
}
