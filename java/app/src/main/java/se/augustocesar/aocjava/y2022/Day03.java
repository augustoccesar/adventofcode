package se.augustocesar.aocjava.y2022;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import se.augustocesar.aocjava.Day;
import se.augustocesar.aocjava.RunnableDay;

@RunnableDay(year = 2022, day = 3)
public class Day03 extends Day {
  @Override
  public String partOne() {
    int sum = 0;

    for (String rucksack : this.readInputLines()) {
      List<Character> items = rucksack.chars().mapToObj(c -> (char) c).toList();

      HashSet<Character> compartment1 = new HashSet<>(items.subList(0, items.size() / 2));
      HashSet<Character> compartment2 =
          new HashSet<>(items.subList(items.size() / 2, items.size()));

      compartment1.retainAll(compartment2);

      if (compartment1.size() != 1) {
        throw new RuntimeException("Should match exactly one item");
      }

      sum += priority(compartment1.iterator().next());
    }

    return String.valueOf(sum);
  }

  @Override
  public String partTwo() {
    int sum = 0;
    List<String> rucksacks = this.readInputLines();

    for (int i = 0; i < rucksacks.size(); i += 3) {
      Set<Character> first =
          rucksacks.get(i).chars().mapToObj(c -> (char) c).collect(Collectors.toSet());

      Set<Character> second =
          rucksacks.get(i + 1).chars().mapToObj(c -> (char) c).collect(Collectors.toSet());

      Set<Character> third =
          rucksacks.get(i + 2).chars().mapToObj(c -> (char) c).collect(Collectors.toSet());

      first.retainAll(second);
      first.retainAll(third);

      if (first.size() != 1) {
        throw new RuntimeException("Should match exactly one item");
      }

      sum += priority(first.iterator().next());
    }

    return String.valueOf(sum);
  }

  private int priority(final char value) {
    byte byteValue = (byte) value;

    if (byteValue >= 65 && byteValue <= 90) { // A-Z
      return byteValue - 38;
    } else if (byteValue >= 97 && byteValue <= 122) { // a-z
      return byteValue - 96;
    } else {
      throw new RuntimeException("Invalid character");
    }
  }
}
