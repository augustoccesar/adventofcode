#include <iostream>
#include <memory>
#include <map>
#include "Command.hpp"
#include "commands/RunCommand.hpp"

class CLI
{
public:
    void registerCommand(std::unique_ptr<Command> command)
    {
        std::string name = command->getName();
        commands_[name] = std::move(command);
    }

    int run(int argc, char *argv[])
    {
        if (argc < 2)
        {
            std::cerr << "Usage: " << argv[0] << " <command> [args...]" << std::endl;
            return 1;
        }

        std::string commandName = argv[1];

        auto it = commands_.find(commandName);
        if (it == commands_.end())
        {
            std::cerr << "Unknown command: " << commandName << std::endl;
            return 1;
        }

        std::vector<std::string> args;
        for (int i = 2; i < argc; ++i)
        {
            args.push_back(argv[i]);
        }

        return it->second->execute(args);
    }

private:
    std::map<std::string, std::unique_ptr<Command>> commands_;
};

int main(int argc, char *argv[])
{
    CLI cli;
    cli.registerCommand(std::make_unique<RunCommand>());
    return cli.run(argc, argv);
}
