package com.augustoccesar.adventofcode;

import com.augustoccesar.adventofcode.day01.Day01;
import com.augustoccesar.adventofcode.day02.Day02;
import com.augustoccesar.adventofcode.day03.Day03;
import com.augustoccesar.adventofcode.day04.Day04;
import com.augustoccesar.adventofcode.day05.Day05;
import com.augustoccesar.adventofcode.day06.Day06;
import com.augustoccesar.adventofcode.day07.Day07;
import com.augustoccesar.adventofcode.day08.Day08;
import com.augustoccesar.adventofcode.day09.Day09;
import com.augustoccesar.adventofcode.day10.Day10;
import com.augustoccesar.adventofcode.day11.Day11;
import com.augustoccesar.adventofcode.day12.Day12;
import com.augustoccesar.adventofcode.day13.Day13;
import com.augustoccesar.adventofcode.day14.Day14;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

public class Main {

  protected static final Map<Integer, Supplier<Task>> availableDays = buildAvailableDaysMap();

  public static void main(String[] args) {
    if (args.length < 1) {
      System.err.println("Invalid amount of args.");
      System.exit(1);
    }

    int dayInt = Integer.parseInt(args[0]);
    final Optional<Task> day = getDay(dayInt);

    if (day.isEmpty()) {
      System.err.println("Day not found.");
      System.exit(1);
      return;
    }

    day.get().run();
  }

  private static HashMap<Integer, Supplier<Task>> buildAvailableDaysMap() {
    HashMap<Integer, Supplier<Task>> availableDays = new HashMap<>();
    availableDays.put(1, Day01::new);
    availableDays.put(2, Day02::new);
    availableDays.put(3, Day03::new);
    availableDays.put(4, Day04::new);
    availableDays.put(5, Day05::new);
    availableDays.put(6, Day06::new);
    availableDays.put(7, Day07::new);
    availableDays.put(8, Day08::new);
    availableDays.put(9, Day09::new);
    availableDays.put(10, Day10::new);
    availableDays.put(11, Day11::new);
    availableDays.put(12, Day12::new);
    availableDays.put(13, Day13::new);
    availableDays.put(14, Day14::new);
    return availableDays;
  }

  public static Optional<Task> getDay(final int dayInt) {
    return Optional.ofNullable(availableDays.get(dayInt).get());
  }
}
