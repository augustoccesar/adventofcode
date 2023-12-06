package se.augustocesar.aoc2019.day01;

import se.augustocesar.aoc2019.Task;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

public class Day01 extends Task {

  @Override
  public String partOne() throws IOException {
    final AtomicInteger sum = new AtomicInteger(0);
    streamInput(
        (s) -> {
          int mass = Integer.parseInt(s);

          int value = sum.get() + fuelRequired(mass);
          sum.set(value);
        });

    return sum.toString();
  }

  @Override
  public String partTwo() throws IOException {
    final AtomicInteger sum = new AtomicInteger(0);
    streamInput(
        (s) -> {
          int mass = Integer.parseInt(s);
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
