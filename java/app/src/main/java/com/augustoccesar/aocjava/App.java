package com.augustoccesar.aocjava;

import com.augustoccesar.aocjava.commands.DayRun;
import picocli.CommandLine;
import picocli.CommandLine.Command;

@Command(subcommands = {DayRun.class})
public class App {
  public static void main(String[] args) {
    System.exit(new CommandLine(new App()).execute(args));
  }
}
