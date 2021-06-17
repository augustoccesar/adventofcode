package com.augustoccesar.adventofcode.day12;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day12 extends Task {

  private static final Pattern COORD_PATTERN = Pattern
      .compile("<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>");

  @Override
  public String partOne() throws IOException {
    List<Moon> moons = parseInput(this.readInput());

    for (int i = 0; i < 1000; i++) {
      for (final Moon moon : moons) {
        moon.applyGravity(moons);
      }

      for (final Moon moon : moons) {
        moon.applyVelocity();
      }
    }

    int totalEnergy = moons.stream()
        .map(Moon::totalEnergy)
        .reduce(Integer::sum)
        .orElseThrow(() -> new RuntimeException("Failed to sum total energy of the moons"));

    return String.valueOf(totalEnergy);
  }

  @Override
  public String partTwo() throws IOException {
    return "-";
  }

  private List<Moon> parseInput(final String input) {
    final List<Moon> moons = new ArrayList<>();

    int currentId = 1;
    for (String line : input.split("\n")) {
      final Matcher matcher = COORD_PATTERN.matcher(line);
      if (matcher.find()) {
        final int x = Integer.parseInt(matcher.group(1));
        final int y = Integer.parseInt(matcher.group(2));
        final int z = Integer.parseInt(matcher.group(3));

        moons.add(Moon.of(currentId, x, y, z));
        currentId++;
      }
    }

    return moons;
  }
}
