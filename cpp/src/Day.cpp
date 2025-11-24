#include "Day.hpp"

std::map<std::pair<int, int>, std::function<std::unique_ptr<Day>()>> &Day::getRegistry()
{
    static std::map<std::pair<int, int>, std::function<std::unique_ptr<Day>()>> registry;
    return registry;
}

std::unique_ptr<Day> Day::getDay(int year, int day)
{
    auto &registry = getRegistry();
    auto key = std::make_pair(year, day);
    auto it = registry.find(key);

    if (it != registry.end())
    {
        return it->second();
    }

    return nullptr;
}

void Day::registerDay(int year, int day, std::function<std::unique_ptr<Day>()> factory)
{
    auto &registry = getRegistry();
    auto key = std::make_pair(year, day);
    registry[key] = factory;
}

std::string Day::readInput(const std::string &suffix) const
{
    std::string filename = "../inputs/" + std::to_string(year_) + "_";

    if (day_ < 10)
    {
        filename += "0";
    }
    filename += std::to_string(day_);

    if (!suffix.empty())
    {
        filename += "_" + suffix;
    }
    filename += ".txt";

    std::ifstream file(filename);
    if (!file.is_open())
    {
        throw std::runtime_error("Could not open file: " + filename);
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}
