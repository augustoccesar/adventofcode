package se.augustocesar.aocjava.y2022;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.IntStream;
import se.augustocesar.aocjava.Day;
import se.augustocesar.aocjava.RunnableDay;

@RunnableDay(year = 2022, day = 1)
public class Day01 extends Day {
  @Override
  public String partOne() {
    return String.valueOf(inventory()[0]);
  }

  @Override
  public String partTwo() {
    return String.valueOf(IntStream.of(Arrays.copyOfRange(inventory(), 0, 3)).sum());
  }

  private int[] inventory() {
    var elfsSupplies = new ArrayList<Integer>();
    elfsSupplies.add(0);

    for (String line : this.readInputLines()) {
      if (line.isEmpty()) {
        elfsSupplies.add(0);
        continue;
      }

      elfsSupplies.set(elfsSupplies.size() - 1, elfsSupplies.getLast() + Integer.parseInt(line));
    }

    Collections.sort(elfsSupplies, Collections.reverseOrder());

    return elfsSupplies.stream().mapToInt(Integer::intValue).toArray();
  }
}
