#include "RunCommand.hpp"
#include <iostream>

int RunCommand::execute(const std::vector<std::string> &args)
{
    if (args.size() != 2)
    {
        std::cerr << "Usage: run <year> <day>" << std::endl;
        return 1;
    }

    try
    {
        int year = std::stoi(args[0]);
        int day = std::stoi(args[1]);

        auto dayInstance = Day::getDay(year, day);
        if (!dayInstance)
        {
            std::cerr << "Day " << day << " not found for year " << year << std::endl;
            return 1;
        }

        std::cout << dayInstance->partOne() << std::endl;
        std::cout << dayInstance->partTwo() << std::endl;

        return 0;
    }
    catch (const std::exception &e)
    {
        std::cerr << "Error parsing arguments: " << e.what() << std::endl;
        return 1;
    }
}
