#include <iostream>
#include <unordered_map>

#include "day01/day01.cpp"
#include "day02/day02.cpp"
#include "day03/day03.cpp"
#include "day04/day04.cpp"
#include "day05/day05.cpp"
#include "day06/day06.cpp"
#include "day07/day07.cpp"
#include "day08/day08.h"
#include "day09/day09.cpp"
#include "day10/day10.cpp"
#include "day11/day11.cpp"
//SETUP:target_include

AbstractTask* getDay(int i_day) {
  switch (i_day) {
    case 1:
      return new Day01;
    case 2:
      return new Day02;
    case 3:
      return new Day03;
    case 4:
      return new Day04;
    case 5:
      return new Day05;
    case 6:
      return new Day06;
    case 7:
      return new Day07;
    case 8:
      return new Day08;
    case 9:
      return new Day09;
    case 10:
      return new Day10;
    case 11:
      return new Day11;
//SETUP:target_case
    default:
      return NULL;
  }
}

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cerr << "Invalid amount of arguments" << std::endl;
    exit(1);
  }

  auto i_day = std::stoi(argv[1]);
  AbstractTask* day = getDay(i_day);

  if (day == nullptr) {
    std::cerr << "Day not found" << std::endl;
    exit(1);
  }

  std::cout << "Part One: " << day->part_one() << std::endl;
  std::cout << "Part Two: " << day->part_two() << std::endl;
  exit(0);
}
