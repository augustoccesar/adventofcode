package com.augustoccesar.adventofcode;

import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Optional;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.junit.Test;

public class MainTest {
  private enum FailureType {
    NOT_FOUND,
    WRONG_VALUE
  }

  @Getter
  @AllArgsConstructor
  private class Failure {
    private int day;
    private int part;
    private FailureType type;
  }

  @Test
  public void checkResults() throws IOException {
    final ArrayList<Failure> failures = new ArrayList<>();
    final InputStream input = this.getClass().getResourceAsStream("/expected_results");
    final BufferedReader br = new BufferedReader(new InputStreamReader(input));

    while (br.ready()) {
      final String line = br.readLine();

      final String[] items = line.split(";");
      final int dayInt = Integer.parseInt(items[0]);
      final String partOne = items[1];
      final String partTwo = items[2];

      final Optional<Task> optDay = Main.getDay(dayInt);
      if (optDay.isEmpty()) {
        failures.add(new Failure(dayInt, 1, FailureType.NOT_FOUND));
        failures.add(new Failure(dayInt, 2, FailureType.NOT_FOUND));
        continue;
      }

      final Task day = optDay.get();

      if (!partOne.equals(day.partOne())) {
        failures.add(new Failure(dayInt, 1, FailureType.WRONG_VALUE));
      }

      if (!partTwo.equals(day.partTwo())) {
        failures.add(new Failure(dayInt, 2, FailureType.WRONG_VALUE));
      }
    }

    if (failures.size() > 0) {
      final StringBuilder message = new StringBuilder();
      for (final Failure failure : failures) {
        message.append(
            String.format(
                "Day %d part %d failed. Reason: %s\n",
                failure.day, failure.part, failure.type.name()));
      }

      fail(message.toString());
    }
  }
}
