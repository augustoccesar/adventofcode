package se.augustocesar.aoc2019;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public abstract class Task {
  public abstract String partOne() throws IOException;

  public abstract String partTwo() throws IOException;

  public void run() {
    try {
      System.out.printf("Part One: %s\n", this.partOne());
      System.out.printf("Part Two: %s\n", this.partTwo());
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  protected interface StreamCallback {
    void call(String line);
  }

  protected String readInput() throws IOException {
    return readInput("input");
  }

  protected String readInput(final String name) throws IOException {
    final StringBuilder stringBuilder = new StringBuilder();
    final String dayName = this.getClass().getSimpleName().toLowerCase();
    final InputStream input =
        this.getClass().getResourceAsStream("/" + dayName + "_" + name + ".txt");
    final BufferedReader br = new BufferedReader(new InputStreamReader(input));

    while (br.ready()) {
      stringBuilder.append(br.readLine()).append("\n");
    }

    return stringBuilder.toString().strip();
  }

  protected void streamInput(StreamCallback streamCallback) throws IOException {
    streamInput("input", streamCallback);
  }

  protected void streamInput(final String name, StreamCallback streamCallback) throws IOException {
    final String dayName = this.getClass().getSimpleName().toLowerCase();
    final InputStream input =
        this.getClass().getResourceAsStream("/" + dayName + "_" + name + ".txt");
    final BufferedReader br = new BufferedReader(new InputStreamReader(input));

    while (br.ready()) {
      streamCallback.call(br.readLine());
    }
  }
}
