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
import java.util.Optional;

public class Main {
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

  public static Optional<Task> getDay(final int dayInt) {
    return switch (dayInt) {
      case 1 -> Optional.of(new Day01());
      case 2 -> Optional.of(new Day02());
      case 3 -> Optional.of(new Day03());
      case 4 -> Optional.of(new Day04());
      case 5 -> Optional.of(new Day05());
      case 6 -> Optional.of(new Day06());
      case 7 -> Optional.of(new Day07());
      case 8 -> Optional.of(new Day08());
      case 9 -> Optional.of(new Day09());
      case 10 -> Optional.of(new Day10());
      case 11 -> Optional.of(new Day11());
      default -> Optional.empty();
    };
  }
}
