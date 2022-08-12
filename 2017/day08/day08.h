#ifndef AOC_DAY08_H
#define AOC_DAY08_H

#include <map>
#include <regex>

#include "../String.h"
#include "../Task.h"

const std::regex instructionPattern(R"((\w+)\s(inc|dec)\s(-?\d+)\sif\s(.+))");
const std::regex conditionPattern(R"((\w+)\s(>|>=|<|<=|==|!=)\s(-?\d+))");

enum Modifier { INCREASE, DECREASE };
Modifier modifierFromStr(const std::string& str);
int applyModifier(int target, Modifier modifier);

enum Cmp {
  EQ,   // ==
  NE,   // !=
  LT,   // <
  LTE,  // <=
  GT,   // >
  GTE   // >=
};
Cmp cmpFromStr(const std::string& str);
bool applyCmp(int left, int right, Cmp cmp);

class Condition {
 public:
  std::string m_register;
  Cmp m_cmp;
  int m_value;

  static Condition fromString(const std::string& str);
};

class Instruction {
 public:
  std::string m_register;
  Modifier m_modifier;
  int m_amount;
  Condition m_condition;

  static Instruction fromString(const std::string& line);
};

class CPU {
 public:
  std::map<std::string, int> m_registry;

  void applyInstruction(const Instruction& instruction);
  void addRegister(std::string register_);
  int largestRegister();
};

class Day08 : public AbstractTask {
 public:
  std::string part_one() override;
  std::string part_two() override;
};

#endif  // AOC_DAY08_H
