#include "../Task.h"
#include "../String.h"

class Day01 : public AbstractTask {
public:
    virtual std::string part_one() override {
        return std::to_string(calculate(input(), 1));
    }

    virtual std::string part_two() override {
        auto data = input();
        int steps = data.length() / 2;
        return std::to_string(calculate(data, steps));
    }

private:
    std::string input() {
        return read_input("inputs/day01_input.txt");
    }

    int calculate(std::string input, int steps) {
        int sum = 0;
        for (size_t i = 0; i < input.length(); i++) {
            int next_idx = (i + steps) % input.length();
            if (input[i] == input[next_idx])
                sum += c_to_digit(input[i]);
        }

        return sum;
    }
};
