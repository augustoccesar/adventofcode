package se.augustocesar.aoc2019.day03;

import se.augustocesar.aoc2019.Task;
import se.augustocesar.aoc2019.utils.Pair;
import se.augustocesar.aoc2019.utils.Point2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Day03 extends Task {

  @Override
  public String partOne() throws IOException {
    final List<HashMap<String, Pair<Point2D, Integer>>> wires = getWiresPaths();

    int shortest = Integer.MAX_VALUE;
    List<Pair<Point2D, Integer>> intersect = intersect(wires.get(0), wires.get(1));

    for (Pair<Point2D, Integer> pointSteps : intersect) {
      final int distance = pointSteps.getLeft().taxicab(Point2D.on(0, 0));
      if (distance < shortest) {
        shortest = distance;
      }
    }

    return String.valueOf(shortest);
  }

  @Override
  public String partTwo() throws IOException {
    final List<HashMap<String, Pair<Point2D, Integer>>> wires = getWiresPaths();

    int shortest = Integer.MAX_VALUE;
    List<Pair<Point2D, Integer>> intersect = intersect(wires.get(0), wires.get(1));

    for (Pair<Point2D, Integer> pointSteps : intersect) {
      final int distance = pointSteps.getRight();
      if (distance < shortest) {
        shortest = distance;
      }
    }

    return String.valueOf(shortest);
  }

  private List<HashMap<String, Pair<Point2D, Integer>>> getWiresPaths() throws IOException {
    List<String> input = this.readInput().lines().collect(Collectors.toList());
    List<HashMap<String, Pair<Point2D, Integer>>> wires = new ArrayList<>();

    for (String in : input) {
      wires.add(generatePath(Point2D.on(0, 0), in));
    }

    return wires;
  }

  private HashMap<String, Pair<Point2D, Integer>> generatePath(
      final Point2D basePoint, final String pathCommands) {
    List<String> commands = List.of(pathCommands.split(","));
    HashMap<String, Pair<Point2D, Integer>> path = new HashMap<>(commands.size());

    int steps = 0;
    Point2D lastPoint = basePoint;
    for (String command : commands) {
      char direction = command.charAt(0);
      int amount = Integer.parseInt(command.substring(1));

      for (int i = 0; i < amount; i++) {
        steps++;
        switch (direction) {
          case 'U' -> lastPoint = lastPoint.moveY(1);
          case 'R' -> lastPoint = lastPoint.moveX(1);
          case 'D' -> lastPoint = lastPoint.moveY(-1);
          case 'L' -> lastPoint = lastPoint.moveX(-1);
          default -> {
            continue;
          }
        }

        path.put(lastPoint.id(), Pair.of(lastPoint, steps));
      }
    }

    return path;
  }

  /**
   * Generates the intersection of two wire paths.
   *
   * <p>The input paths consists of: {@link HashMap} of {@link String} (Key of the {@link Point2D})
   * to a {@link Pair} of {@link Point2D} and {@link Integer} (steps taken to reach the
   * {@link Point2D}).
   *
   * @param pathOne Path of the first wire.
   * @param pathTwo Path of the second wire.
   * @return The intersection of both paths paired with the summed steps to get to the
   * {@link Point2D} by both wires.
   */
  private List<Pair<Point2D, Integer>> intersect(
      final HashMap<String, Pair<Point2D, Integer>> pathOne,
      final HashMap<String, Pair<Point2D, Integer>> pathTwo) {
    List<Pair<Point2D, Integer>> intersection = new ArrayList<>();

    for (Map.Entry<String, Pair<Point2D, Integer>> entry : pathOne.entrySet()) {
      Pair<Point2D, Integer> pathTwoEntry = pathTwo.get(entry.getValue().getLeft().id());
      if (pathTwoEntry != null) {
        intersection.add(
            Pair.of(
                entry.getValue().getLeft(), entry.getValue().getRight() + pathTwoEntry.getRight()));
      }
    }

    return intersection;
  }
}
