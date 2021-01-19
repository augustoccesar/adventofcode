package com.augustoccesar.adventofcode.day04;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Day04 extends Task {
  @Override
  public String partOne() throws IOException {
    return String.valueOf(getValidCount(1));
  }

  @Override
  public String partTwo() throws IOException {
    return String.valueOf(getValidCount(2));
  }

  private int getValidCount(int version) throws IOException {
    final String[] input = this.readInput().strip().split("-");
    final int rangeStart = Integer.parseInt(input[0]);
    final int rangeEnd = Integer.parseInt(input[1]);

    AtomicInteger valid = new AtomicInteger(0);

    IntStream.rangeClosed(rangeStart, rangeEnd)
        .forEach(
            (trying) -> {
              if (isValid(trying, version)) {
                valid.incrementAndGet();
              }
            });

    return valid.get();
  }

  private boolean isValid(int password, int version) {
    String stringPass = String.valueOf(password);
    List<Character> passChars =
        stringPass.chars().mapToObj((c) -> (char) c).collect(Collectors.toList());

    Set<String> pairs = new HashSet<>();

    for (int i = 0; i < passChars.size(); i++) {
      final int currentValue = Character.getNumericValue(passChars.get(i));

      if (i == 0) {
        continue;
      }

      int previousValue = Character.getNumericValue(passChars.get(i - 1));

      if (currentValue > previousValue) {
        continue;
      }

      if (currentValue == previousValue) {
        pairs.add(String.valueOf(currentValue).repeat(2));
        continue;
      }

      return false;
    }

    return switch (version) {
      case 1 -> pairs.size() > 0;
      case 2 -> pairs.stream()
          .anyMatch(pair -> stringPass.indexOf(pair) == stringPass.lastIndexOf(pair));
      default -> false;
    };
  }
}
