package se.augustocesar.aocjava;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import se.augustocesar.aocjava.commands.DayRun;
import se.augustocesar.aocjava.commands.Days;

@Command(subcommands = {DayRun.class, Days.class})
public class App {
  public static void main(String[] args) {
    System.exit(new CommandLine(new App()).execute(args));
  }
}
