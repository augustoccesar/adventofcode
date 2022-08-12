#include "day08.h"

std::string Day08::part_one() {
  std::string input = read_input("inputs/day08_input.txt");
  std::vector<std::string> lines = split(input, '\n');

  CPU cpu = CPU();
  std::vector<Instruction> instructions;

  for (std::string line : lines) {
    auto instruction = Instruction::fromString(line);
    instructions.push_back(instruction);

    cpu.addRegister(instruction.m_register);
    cpu.addRegister(instruction.m_condition.m_register);
  }

  for (Instruction& instruction : instructions) {
    cpu.applyInstruction(instruction);
  }

  return std::to_string(cpu.largestRegister());
}

std::string Day08::part_two() { return "-"; }

Modifier modifierFromStr(const std::string& str) {
  if (str == "inc")
    return Modifier::INCREASE;
  else if (str == "dec")
    return Modifier::DECREASE;
  else
    throw std::invalid_argument("invalid value for modifier");
}

int applyModifier(int target, int value, Modifier modifier) {
  if (modifier == Modifier::INCREASE)
    return target + value;
  else if (modifier == Modifier::DECREASE)
    return target - value;
  else
    throw std::invalid_argument("unsupported modifier");
}

Cmp cmpFromStr(const std::string& str) {
  if (str == "==")
    return Cmp::EQ;
  else if (str == "!=")
    return Cmp::NE;
  else if (str == "<")
    return Cmp::LT;
  else if (str == "<=")
    return Cmp::LTE;
  else if (str == ">")
    return Cmp::GT;
  else if (str == ">=")
    return Cmp::GTE;
  else
    throw std::invalid_argument("invalid value for comparison");
}

bool applyCmp(int left, int right, Cmp cmp) {
  if (cmp == Cmp::EQ)
    return left == right;
  else if (cmp == Cmp::NE)
    return left != right;
  else if (cmp == Cmp::LT)
    return left < right;
  else if (cmp == Cmp::LTE)
    return left <= right;
  else if (cmp == Cmp::GT)
    return left > right;
  else if (cmp == Cmp::GTE)
    return left >= right;
  else
    throw std::invalid_argument("unsupported comparison");
}

Condition Condition::fromString(const std::string& str) {
  std::smatch matches;
  std::regex_search(str, matches, conditionPattern);

  std::string registerMatch = matches[1];
  std::string cmpMatch = matches[2];
  std::string valueMatch = matches[3];

  return {.m_register = registerMatch,
          .m_cmp = cmpFromStr(cmpMatch),
          .m_value = std::stoi(valueMatch)};
}

Instruction Instruction::fromString(const std::string& line) {
  std::smatch matches;
  std::regex_search(line, matches, instructionPattern);

  std::string registerMatch = matches[1];
  std::string valueModifierMatch = matches[2];
  std::string amountMatch = matches[3];
  std::string conditionMatch = matches[4];

  return {.m_register = registerMatch,
          .m_modifier = modifierFromStr(valueModifierMatch),
          .m_amount = stoi(amountMatch),
          .m_condition = Condition::fromString(conditionMatch)};
}

void CPU::applyInstruction(const Instruction& instruction) {
  auto condition = instruction.m_condition;
  bool fulfilCondition = applyCmp(m_registry[condition.m_register],
                                  condition.m_value, condition.m_cmp);
  if (fulfilCondition) {
    int currentValue = m_registry[instruction.m_register];
    m_registry[instruction.m_register] = applyModifier(
        currentValue, instruction.m_amount, instruction.m_modifier);
  }
}

void CPU::addRegister(std::string register_) {
  if (m_registry.contains(register_)) {
    return;
  }

  m_registry[register_] = 0;
}

int CPU::largestRegister() {
  int max = INT_MIN;
  for (auto const& [key, val] : m_registry) {
    if(val > max) {
      max = val;
    }
  }

  return max;
}
