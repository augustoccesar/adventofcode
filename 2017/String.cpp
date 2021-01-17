#include "String.h"

#include <sstream>

int c_to_digit(char c) { return (c - '0'); }

std::vector<std::string> split(const std::string& str, char delimiter) {
  std::vector<std::string> tokens;
  std::string token;
  std::istringstream tokenStream(str);

  while (std::getline(tokenStream, token, delimiter)) tokens.push_back(token);

  return tokens;
}
