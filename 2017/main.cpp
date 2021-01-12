#include <iostream>
#include <unordered_map>

#include "day01/day01.cpp"
#include "day02/day02.cpp"
//#include "day03/day03.cpp"
#include "day04/day04.cpp"
#include "day05/day05.cpp"


int main(int argc, char *argv[]) {
    std::unordered_map<int, AbstractTask *> day_map;

    auto day01 = Day01{};
    auto day02 = Day02{};
//    auto day03 = Day03{};
    auto day04 = Day04{};
    auto day05 = Day05{};

    day_map[1] = &day01;
    day_map[2] = &day02;
//    day_map[3] = &day03;
    day_map[4] = &day04;
    day_map[5] = &day05;

    if (argc < 2) {
        std::cerr << "Invalid amount of arguments" << std::endl;
        exit(1);
    }

    auto i_day = std::stoi(argv[1]);

    if (day_map.contains(i_day)) {
        auto day = day_map[i_day];

        std::cout << "Part One: " << day->part_one() << std::endl;
        std::cout << "Part Two: " << day->part_two() << std::endl;
        exit(0);
    } else {
        std::cerr << "Day not found" << std::endl;
        exit(1);
    }
}
