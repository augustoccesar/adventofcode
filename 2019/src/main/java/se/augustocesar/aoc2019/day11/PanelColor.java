package se.augustocesar.aoc2019.day11;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
enum PanelColor {
  BLACK(0, '.'),
  WHITE(1, '#');

  @Getter
  private final int intRepr;

  @Getter
  private final char charRepr;

  public static PanelColor from(final int integer) {
    return switch (integer) {
      case 0 -> PanelColor.BLACK;
      case 1 -> PanelColor.WHITE;
      default -> throw new IllegalStateException("Unexpected value: " + integer);
    };
  }
}
