package se.augustocesar.aoc2019.day01;

import java.util.concurrent.atomic.AtomicInteger;
import se.augustocesar.aoc2019.task.RunnableTask;
import se.augustocesar.aoc2019.task.Task;

@RunnableTask(day = 1)
public class Day01 extends Task {

  @Override
  public String partOne() {
    final AtomicInteger sum = new AtomicInteger(0);
    this.readInputLines().forEach(line -> {
      int mass = Integer.parseInt(line);

      int value = sum.get() + fuelRequired(mass);
      sum.set(value);
    });

    return sum.toString();
  }

  @Override
  public String partTwo() {
    final AtomicInteger sum = new AtomicInteger(0);
    this.readInputLines().forEach(line -> {
      int mass = Integer.parseInt(line);
      int fuelRequired = fuelRequired(mass);
      int stageSum = fuelRequired;

      while (true) {
        fuelRequired = fuelRequired(fuelRequired);
        if (fuelRequired <= 0) {
          break;
        }

        stageSum += fuelRequired;
      }

      sum.set(sum.get() + stageSum);
    });

    return sum.toString();
  }

  private static int fuelRequired(int mass) {
    return (mass / 3) - 2;
  }
}
