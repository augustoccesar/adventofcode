#include <map>
#include <regex>

#include "../String.h"
#include "../Task.h"

class Day07 : public AbstractTask {
 public:
  std::string part_one() override {
    std::string input = read_input("inputs/day07_input.txt");

    std::vector<std::string> root_names;
    std::map<std::string, bool> connected_to_root_names;

    for (auto line : split(input, '\n')) {
      std::regex rgx("(\\w+)\\s\\((\\d+)\\)(?:\\s->\\s(.+))?");
      std::smatch matches;

      if (std::regex_search(line, matches, rgx)) {
        std::string programs_raw = matches[3];
        if (programs_raw != "") {
          programs_raw.erase(
              std::remove(programs_raw.begin(), programs_raw.end(), ' '),
              programs_raw.end());
          std::vector<std::string> nested_programs = split(programs_raw, ',');

          root_names.push_back(matches[1]);
          for (auto nested_program : nested_programs) {
            connected_to_root_names[nested_program] = true;
          }
        }
      }
    }

    for (auto root : root_names) {
      bool found = connected_to_root_names[root];
      if (!found) {
        return root;
      }
    }

    throw "unreachable";
  }

  std::string part_two() override { return "-"; }
};
