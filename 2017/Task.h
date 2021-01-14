#ifndef AOC_TASK_H
#define AOC_TASK_H

#include<fstream>
#include<sstream>

class AbstractTask {
public:
    virtual std::string part_one() = 0;

    virtual std::string part_two() = 0;

protected:
    std::string read_input(std::string path) {
        std::ifstream input(path);
        std::stringstream sstr;

        while (input >> sstr.rdbuf());

        return sstr.str();
    }
};

#endif //AOC_TASK_H
