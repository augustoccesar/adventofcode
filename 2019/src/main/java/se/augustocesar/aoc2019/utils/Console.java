package se.augustocesar.aoc2019.utils;

public class Console {
  public static void clearScreen() {
    System.out.print("\033[H\033[2J");
    System.out.flush();
  }
}
