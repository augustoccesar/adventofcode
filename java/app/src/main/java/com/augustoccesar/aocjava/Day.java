package com.augustoccesar.aocjava;

import com.google.common.base.Strings;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public abstract class Day {
  public abstract String partOne();

  public abstract String partTwo();

  protected int getYear() {
    RunnableDay annotation = this.getClass().getAnnotation(RunnableDay.class);
    return annotation != null ? annotation.year() : 0;
  }

  protected int getDay() {
    RunnableDay annotation = this.getClass().getAnnotation(RunnableDay.class);
    return annotation != null ? annotation.day() : 0;
  }

  protected List<String> readInputLines() {
    return readInputLines(null);
  }

  protected List<String> readInputLines(final String name) {
    final String fileName = fileName(name);

    File file = new File(String.format("../../inputs/%s", fileName));
    try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
      return reader.lines().toList();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  protected String readInput() {
    return readInput(null);
  }

  protected String readInput(final String name) {
    return String.join("", readInputLines(name));
  }

  private String fileName(final String name) {
    if (name == null) {
      return this.getYear()
          + "_"
          + Strings.padStart(String.valueOf(this.getDay()), 2, '0')
          + ".txt";
    } else {
      return this.getYear()
          + "_"
          + Strings.padStart(String.valueOf(this.getDay()), 2, '0')
          + "_"
          + name
          + ".txt";
    }
  }
}
