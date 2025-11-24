package com.augustoccesar.aocjava.commands;

import com.augustoccesar.aocjava.Day;
import com.augustoccesar.aocjava.DayLoader;
import java.util.Optional;
import java.util.concurrent.Callable;
import picocli.CommandLine.Command;
import picocli.CommandLine.Parameters;

@Command(name = "run")
public class DayRun implements Callable<Integer> {
  @Parameters(index = "0")
  int year;

  @Parameters(index = "1")
  int day;

  @Override
  public Integer call() throws Exception {
    var dayLoader = DayLoader.load();
    Optional<Day> day = dayLoader.getDay(this.year, this.day);

    if (day.isEmpty()) {
      System.out.printf("Day %d for year %d not found", this.day, this.year);
      return 1;
    }

    System.out.println(day.get().partOne());
    System.out.println(day.get().partTwo());

    return 0;
  }
}
