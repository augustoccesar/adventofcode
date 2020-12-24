#include "../base.cpp"

bool contain_repeated_words(std::string passphrase);
bool contain_anagram(std::string passphrase);
bool is_anagram(std::string str1, std::string str2);

void part_one(std::string input_file)
{
    auto input = read_input(input_file);
    auto passwords = split(input, '\n');

    int valid_count = 0;
    for (auto password : passwords)
    {
        bool valid = true;

        if (contain_repeated_words(password))
            valid = false;

        if (valid)
            valid_count++;
    }

    std::cout << "Part One: " << valid_count << std::endl;
}

void part_two(std::string input_file)
{
    auto input = read_input(input_file);
    auto passphrases = split(input, '\n');

    int valid_count = 0;
    for (auto passphrase : passphrases)
    {
        bool valid = true;

        if (contain_repeated_words(passphrase) || contain_anagram(passphrase))
            valid = false;

        if (valid)
            valid_count++;
    }

    std::cout << "Part Two: " << valid_count << std::endl;
}

bool contain_repeated_words(std::string passphrase)
{
    std::map<std::string, bool> seen_words;
    bool found_repeated = false;
    for (auto word : split(passphrase, ' '))
    {
        if (seen_words[word])
        {
            found_repeated = true;
            break;
        }

        seen_words[word] = true;
    }

    return found_repeated;
}

bool contain_anagram(std::string passphrase)
{
    auto words = split(passphrase, ' ');
    for (size_t i = 0; i < words.size(); i++)
    {
        for (size_t j = i + 1; j < words.size(); j++)
        {
            if (is_anagram(words[i], words[j]))
            {
                return true;
            }
        }
    }

    return false;
}

bool is_anagram(std::string str1, std::string str2)
{
    if (str1.length() != str2.length())
        return false;

    std::map<char, int> char_count_str1;
    std::map<char, int> char_count_str2;

    std::vector<char> chars1(str1.begin(), str1.end());
    std::vector<char> chars2(str2.begin(), str2.end());

    for (char str1_char : chars1)
    {
        char_count_str1[str1_char]++;
    }
    for (char str2_char : chars2)
    {
        char_count_str2[str2_char]++;
    }

    for (auto const &[key, val] : char_count_str1)
    {
        if (char_count_str2[key] != val)
        {
            return false;
        }
    }

    return true;
}

// --------------------------------------------------------------------------------------------------------------------

int main()
{
    auto input_file = "./input.txt";
    part_one(input_file);
    part_two(input_file);

    return 0;
}
