#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <map>

#define c_to_digit(c) (c - '0')

std::string read_input(std::string path)
{
    std::ifstream input(path);
    std::stringstream sstr;

    while (input >> sstr.rdbuf())
        ;

    return sstr.str();
}

std::vector<std::string> split(std::string str, char delimiter)
{
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(str);

    while (std::getline(tokenStream, token, delimiter))
        tokens.push_back(token);

    return tokens;
}
