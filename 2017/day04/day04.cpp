#include <map>
#include <utility>

#include "../String.h"
#include "../Task.h"

class Day04 : public AbstractTask {
 public:
  std::string part_one() override {
    auto input = read_input("inputs/day04_input.txt");
    auto passwords = split(input, '\n');

    int valid_count = 0;
    for (const auto& password : passwords) {
      bool valid = true;

      if (contain_repeated_words(password)) valid = false;

      if (valid) valid_count++;
    }

    return std::to_string(valid_count);
  }

  std::string part_two() override {
    auto input = read_input("inputs/day04_input.txt");
    auto passphrases = split(input, '\n');

    int valid_count = 0;
    for (const auto& passphrase : passphrases) {
      bool valid = true;

      if (contain_repeated_words(passphrase) || contain_anagram(passphrase))
        valid = false;

      if (valid) valid_count++;
    }

    return std::to_string(valid_count);
  }

 private:
  static bool contain_repeated_words(const std::string& passphrase) {
    std::map<std::string, bool> seen_words;
    bool found_repeated = false;
    for (const auto& word : split(passphrase, ' ')) {
      if (seen_words[word]) {
        found_repeated = true;
        break;
      }

      seen_words[word] = true;
    }

    return found_repeated;
  }

  static bool contain_anagram(const std::string& passphrase) {
    auto words = split(passphrase, ' ');
    for (size_t i = 0; i < words.size(); i++) {
      for (size_t j = i + 1; j < words.size(); j++) {
        if (is_anagram(words[i], words[j])) {
          return true;
        }
      }
    }

    return false;
  }

  static bool is_anagram(std::string str1, std::string str2) {
    if (str1.length() != str2.length()) return false;

    std::map<char, int> char_count_str1;
    std::map<char, int> char_count_str2;

    std::vector<char> chars1(str1.begin(), str1.end());
    std::vector<char> chars2(str2.begin(), str2.end());

    for (char str1_char : chars1) {
      char_count_str1[str1_char]++;
    }
    for (char str2_char : chars2) {
      char_count_str2[str2_char]++;
    }

    for (auto const& [key, val] : char_count_str1) {
      if (char_count_str2[key] != val) {
        return false;
      }
    }

    return true;
  }
};
