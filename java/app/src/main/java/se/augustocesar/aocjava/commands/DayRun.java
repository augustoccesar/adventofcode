package se.augustocesar.aocjava.commands;

import java.util.Optional;
import java.util.concurrent.Callable;
import picocli.CommandLine.Command;
import picocli.CommandLine.Parameters;
import se.augustocesar.aocjava.Day;
import se.augustocesar.aocjava.DayRegistry;

@Command(name = "run")
public class DayRun implements Callable<Integer> {
  @Parameters(index = "0")
  int year;

  @Parameters(index = "1")
  int day;

  @Override
  public Integer call() throws Exception {
    var dayLoader = DayRegistry.load();
    Optional<Day> day = dayLoader.getDay(this.year, this.day);

    if (day.isEmpty()) {
      System.out.printf("Day %d for year %d not found", this.day, this.year);
      return 1;
    }

    long partOneStart = System.nanoTime();
    String resultPartOne = day.get().partOne();
    long partOneEnd = System.nanoTime();

    System.out.printf("%s;%d\n", resultPartOne, partOneEnd - partOneStart);

    long partTwoStart = System.nanoTime();
    String resultPartTwo = day.get().partTwo();
    long partTwoEnd = System.nanoTime();

    System.out.printf("%s;%d\n", resultPartTwo, partTwoEnd - partTwoStart);

    return 0;
  }
}
