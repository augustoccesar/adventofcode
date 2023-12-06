package se.augustocesar.aoc2019.day11;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
enum Turn {
  LEFT(0),
  RIGHT(1);

  @Getter
  private final int intRepr;

  public static Turn from(final int integer) {
    return switch (integer) {
      case 0 -> LEFT;
      case 1 -> RIGHT;
      default -> throw new IllegalStateException("Unexpected value: " + integer);
    };
  }
}
