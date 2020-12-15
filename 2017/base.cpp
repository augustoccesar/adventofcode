#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>

#define c_to_digit(c) (c - '0')

std::string read_input(std::string path)
{
    std::ifstream input(path);
    std::stringstream sstr;

    while (input >> sstr.rdbuf())
        ;

    return sstr.str();
}
