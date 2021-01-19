package com.augustoccesar.adventofcode;

import com.augustoccesar.adventofcode.day01.Day01;
import com.augustoccesar.adventofcode.day02.Day02;
import com.augustoccesar.adventofcode.day03.Day03;
import com.augustoccesar.adventofcode.day04.Day04;
import com.augustoccesar.adventofcode.day05.Day05;
import com.augustoccesar.adventofcode.day06.Day06;

public class Main {
  public static void main(String[] args) {
    if (args.length < 1) {
      System.err.println("Invalid amount of args.");
      System.exit(1);
    }

    int dayInt = Integer.parseInt(args[0]);
    
    final Task day;
    switch (dayInt) {
      case 1 -> day = new Day01();
      case 2 -> day = new Day02();
      case 3 -> day = new Day03();
      case 4 -> day = new Day04();
      case 5 -> day = new Day05();
      case 6 -> day = new Day06();
      default -> {
        System.err.println("Day not found.");
        System.exit(1);
        return;
      }
    }

    day.run();
  }
}
