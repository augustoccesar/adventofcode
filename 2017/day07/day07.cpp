#include <iostream>
#include <map>
#include <regex>

#include "../String.h"
#include "../Task.h"

struct Disc {
  std::string name;
  int weight;
  std::vector<std::string> child_discs_names;
};

struct ParsedInput {
  std::map<std::string, Disc> discs_map;
  Disc root;
};

class Day07 : public AbstractTask {
 public:
  std::string part_one() override {
    std::string input = read_input("inputs/day07_input.txt");
    ParsedInput parsed_input = parse_input(input);

    return parsed_input.root.name;
  }

  std::string part_two() override {
    std::string input = read_input("inputs/day07_input.txt");
    ParsedInput parsed_input = parse_input(input);

    int response = -1;
    weighting(&parsed_input.root, parsed_input.discs_map, &response);

    return std::to_string(response);
  }

 private:
  Disc parse_line(std::string line) {
    std::regex rgx("(\\w+)\\s\\((\\d+)\\)(?:\\s->\\s(.+))?");
    std::smatch matches;

    if (std::regex_search(line, matches, rgx)) {
      std::string nested_discs = matches[3];
      std::vector<std::string> child_discs_names;
      if (nested_discs != "") {
        nested_discs.erase(
            std::remove(nested_discs.begin(), nested_discs.end(), ' '),
            nested_discs.end());
        child_discs_names = split(nested_discs, ',');
      }

      return {.name = matches[1],
              .weight = stoi(matches[2]),
              .child_discs_names = child_discs_names};
    }

    throw "invalid line";
  }

  ParsedInput parse_input(std::string input) {
    std::map<std::string, Disc> discs_map;
    std::map<std::string, std::tuple<bool, bool>> child_tracking;

    for (auto line : split(input, '\n')) {
      Disc disc = parse_line(line);
      discs_map[disc.name] = disc;

      bool has_children = disc.child_discs_names.size() > 0;
      child_tracking[disc.name] = std::make_tuple(has_children, false);
    }

    for (auto &[disc_name, disc] : discs_map) {
      if (disc.child_discs_names.size() > 0) {
        for (auto child_name : disc.child_discs_names) {
          Disc *child_disc = &discs_map[child_name];

          auto a = &child_tracking[child_disc->name];
          std::get<1>(*a) = true;
        }
      }
    }

    Disc root;
    for (auto &[disc_name, disc] : discs_map) {
      std::pair<bool, bool> tracking = child_tracking[disc_name];
      if (std::get<0>(tracking) && !std::get<1>(tracking)) {
        root = disc;
        break;
      }
    }

    return {
        .discs_map = discs_map,
        .root = root,
    };
  }

  int weighting(Disc *base, std::map<std::string, Disc> discs_map,
                int *response) {
    if (base->child_discs_names.size() == 0) {
      return base->weight;
    }

    std::vector<int> child_weights(base->child_discs_names.size());
    int total_child_weight = 0;
    std::map<int, int> occurrences;
    for (int i = 0; i < base->child_discs_names.size(); i++) {
      Disc child = discs_map[base->child_discs_names[i]];
      int weight = weighting(&child, discs_map, response);
      child_weights.at(i) = weight;
      total_child_weight += weight;

      occurrences[weight] += 1;
    }

    if (occurrences.size() != 1) {
      int correct_weight;
      Disc out_of_weight;
      int wrong_weight;

      for (auto [weight, occurrences] : occurrences) {
        if (occurrences == 1) {
          for (int i = 0; i < child_weights.size(); i++) {
            if (child_weights[i] == weight) {
              out_of_weight = discs_map[base->child_discs_names[i]];
              wrong_weight = weight;
              break;
            }
          }
        } else {
          correct_weight = weight;
        }
      }

      int diff_weight = correct_weight - wrong_weight;
      int res = out_of_weight.weight + diff_weight;

      if (*response == -1) {
        *response = res;
      }
    }

    return base->weight + total_child_weight;
  }
};
