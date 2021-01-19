package com.augustoccesar.adventofcode;

import com.augustoccesar.adventofcode.day01.Day01;
import com.augustoccesar.adventofcode.day02.Day02;
import com.augustoccesar.adventofcode.day03.Day03;
import com.augustoccesar.adventofcode.day04.Day04;
import com.augustoccesar.adventofcode.day05.Day05;
import com.augustoccesar.adventofcode.day06.Day06;
import java.util.Map;

public class Main {
  public static void main(String[] args) {
    Map<Integer, Task> days =
        Map.of(
            1, new Day01(),
            2, new Day02(),
            3, new Day03(),
            4, new Day04(),
            5, new Day05(),
            6, new Day06());

    if (args.length < 1) {
      System.err.println("Invalid amount of args.");
      System.exit(1);
    }

    int dayInt = Integer.parseInt(args[0]);
    final Task day = days.get(dayInt);

    if (day == null) {
      System.err.println("Day not found.");
      System.exit(1);
    }

    day.run();
  }
}
