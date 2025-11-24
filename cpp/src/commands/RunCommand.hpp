#pragma once

#include "../Command.hpp"
#include "../Day.hpp"

class RunCommand : public Command
{
public:
    std::string getName() const override { return "run"; }
    int execute(const std::vector<std::string> &args) override;
};
