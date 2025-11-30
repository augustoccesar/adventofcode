package com.augustoccesar.aocjava;

import com.augustoccesar.aocjava.commands.DayRun;
import com.augustoccesar.aocjava.commands.Days;
import picocli.CommandLine;
import picocli.CommandLine.Command;

@Command(subcommands = {DayRun.class, Days.class})
public class App {
  public static void main(String[] args) {
    System.exit(new CommandLine(new App()).execute(args));
  }
}
