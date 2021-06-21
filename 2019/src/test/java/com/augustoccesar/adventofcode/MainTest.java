package com.augustoccesar.adventofcode;

import static org.junit.Assert.fail;

import com.augustoccesar.adventofcode.utils.Pair;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.junit.Test;

public class MainTest {

  private enum FailureType {
    IMPLEMENTATION_NOT_FOUND,
    EXPECTED_RESULT_NOT_FOUND,
    RESULT_MISMATCH,
  }

  @Getter
  @AllArgsConstructor
  private static class Failure {

    private final int day;
    private final int part;
    private final FailureType type;
  }

  @Test
  public void checkResults() throws IOException {
    final ArrayList<Failure> failures = new ArrayList<>();
    final InputStream input = this.getClass().getResourceAsStream("/expected_results");
    final BufferedReader br = new BufferedReader(new InputStreamReader(input));

    final Map<Integer, Supplier<Task>> implementedDays = Main.availableDays;
    final Map<Integer, Pair<String, String>> expectedResults = new HashMap<>();

    while (br.ready()) {
      final String line = br.readLine();
      final String[] items = line.split(";");
      expectedResults.put(Integer.parseInt(items[0]), Pair.of(items[1], items[2]));
    }

    final Set<Integer> days = new HashSet<>();
    days.addAll(implementedDays.keySet());
    days.addAll(expectedResults.keySet());

    for (final Integer day : days) {
      final List<Failure> preRunFailures = new ArrayList<>();
      final Supplier<Task> implementation = implementedDays.get(day);
      final Pair<String, String> expectedResult = expectedResults.get(day);

      if (implementation == null) {
        preRunFailures.add(new Failure(day, 1, FailureType.IMPLEMENTATION_NOT_FOUND));
        preRunFailures.add(new Failure(day, 2, FailureType.IMPLEMENTATION_NOT_FOUND));
      }

      if (expectedResult == null) {
        preRunFailures.add(new Failure(day, 1, FailureType.EXPECTED_RESULT_NOT_FOUND));
        preRunFailures.add(new Failure(day, 2, FailureType.EXPECTED_RESULT_NOT_FOUND));
      }

      if (preRunFailures.size() > 0) {
        failures.addAll(preRunFailures);
        continue;
      }

      final Task task = implementation.get();
      if (!expectedResult.getLeft().equals(task.partOne())) {
        failures.add(new Failure(day, 1, FailureType.RESULT_MISMATCH));
      }

      if (!expectedResult.getRight().equals(task.partTwo())) {
        failures.add(new Failure(day, 1, FailureType.RESULT_MISMATCH));
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
