package se.augustocesar.aoc2019.day14;

import se.augustocesar.aoc2019.task.RunnableTask;
import se.augustocesar.aoc2019.task.Task;

@RunnableTask(day = 14)
public class Day14 extends Task {

  @Override
  public String partOne() {
    NanoFactory nanoFactory = NanoFactory.fromSpec(this.readInputLines());
    long totalOfOres = nanoFactory.produce(1, "FUEL");

    return String.valueOf(totalOfOres);
  }

  @Override
  public String partTwo() {
    long target = 1_000_000_000_000L;
    String input = this.readInput();
    NanoFactory nanoFactory = NanoFactory.fromSpec(this.readInputLines());

    int count = 1_000_000; // It can't be less than a million
    while (true) { // Do an outer loop jumping a lot
      long totalOfOres = nanoFactory.produce(count, "FUEL");
      if (totalOfOres > target) { // Once it passes the value, do a more granular inverse loop
        for (int i = count; ; i--) {
          totalOfOres = nanoFactory.produce(i, "FUEL");
          if (totalOfOres < target) {
            count = i;
            break;
          }
        }
        break;
      }

      count += 1_000;
    }

    return String.valueOf(count);
  }
}
