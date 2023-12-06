package se.augustocesar.aoc2019.day12;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import se.augustocesar.aoc2019.task.RunnableTask;
import se.augustocesar.aoc2019.task.Task;

@RunnableTask(day = 12)
public class Day12 extends Task {

  private static final Pattern COORD_PATTERN = Pattern
      .compile("<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>");

  @Override
  public String partOne() {
    List<Moon> moons = parseInput(this.readInputLines());
    Orbit orbit = Orbit.with(moons);

    for (int i = 0; i < 1000; i++) {
      orbit.tick();
    }

    int totalEnergy = orbit.energy();

    return String.valueOf(totalEnergy);
  }

  @Override
  public String partTwo() {
    return "-";
  }

  private List<Moon> parseInput(final List<String> input) {
    final List<Moon> moons = new ArrayList<>();

    int currentId = 1;
    for (String line : input) {
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
