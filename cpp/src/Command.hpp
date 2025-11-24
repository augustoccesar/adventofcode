#pragma once

#include <vector>
#include <string>
#include <memory>

class Command
{
public:
    virtual ~Command() = default;
    virtual std::string getName() const = 0;
    virtual int execute(const std::vector<std::string> &args) = 0;
};
