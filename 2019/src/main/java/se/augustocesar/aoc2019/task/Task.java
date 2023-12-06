package se.augustocesar.aoc2019.task;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;

public abstract class Task {

  public abstract String partOne();

  public abstract String partTwo();

  public void run() {
    System.out.printf("Part One: %s\n", this.partOne());
    System.out.printf("Part Two: %s\n", this.partTwo());
  }

  protected List<String> readInputLines() {
    return readInputLines("input");
  }

  protected List<String> readInputLines(final String name) {
    final String resourceName = resourceName(name);
    final InputStream input = this.getClass().getResourceAsStream("/" + resourceName);
    final BufferedReader reader = new BufferedReader(new InputStreamReader(input));

    return reader.lines().toList();
  }

  protected String readInput() {
    return readInput("input");
  }

  protected String readInput(final String name) {
    return String.join("", readInputLines(name));
  }

  private String resourceName(final String name) {
    final String dayName = this.getClass().getSimpleName().toLowerCase();
    return dayName + "_" + name + ".txt";
  }
}
