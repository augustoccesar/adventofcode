package com.augustoccesar.adventofcode.utils;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MinMax<T extends Comparable<T>> {

  @Getter private T min;
  @Getter private T max;

  public static <T extends Comparable<T>> MinMax<T> from(final T[] items) {
    if (items.length <= 0) {
      return null; // For now I'm fine with this. TODO: Take a second look into it
    }

    T min = items[0];
    T max = items[0];

    for (T item : items) {
      if (item.compareTo(min) < 0) {
        min = item;
      }

      if (item.compareTo(max) > 0) {
        max = item;
      }
    }

    return new MinMax<>(min, max);
  }
}
