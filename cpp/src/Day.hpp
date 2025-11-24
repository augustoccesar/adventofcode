#pragma once

#include <string>
#include <memory>
#include <map>
#include <fstream>
#include <sstream>
#include <functional>

class Day
{
public:
    virtual ~Day() = default;
    virtual std::string partOne() = 0;
    virtual std::string partTwo() = 0;
    static std::unique_ptr<Day> getDay(int year, int day);
    static void registerDay(int year, int day, std::function<std::unique_ptr<Day>()> factory);

protected:
    Day(int year, int day) : year_(year), day_(day) {}
    std::string readInput(const std::string &suffix = "") const;

private:
    int year_;
    int day_;
    static std::map<std::pair<int, int>, std::function<std::unique_ptr<Day>()>> &getRegistry();
};

#define REGISTER_DAY(Year, DayNum, ClassName)                                                                                           \
    namespace                                                                                                                           \
    {                                                                                                                                   \
        struct Register##ClassName                                                                                                      \
        {                                                                                                                               \
            Register##ClassName()                                                                                                       \
            {                                                                                                                           \
                ::Day::registerDay(Year, DayNum, []() -> std::unique_ptr<::Day> { return std::make_unique<ClassName>(Year, DayNum); }); \
            }                                                                                                                           \
        };                                                                                                                              \
        static Register##ClassName register##ClassName;                                                                                 \
    }
