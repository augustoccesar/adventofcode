package com.augustoccesar.adventofcode.day10;

import com.augustoccesar.adventofcode.Task;
import com.augustoccesar.adventofcode.utils.Point2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

public class Day10 extends Task {
  @Override
  public String partOne() throws IOException {
    ArrayList<Point2D> asteroids = this.getAsteroids(this.readInput());
    int maxVisible = 0;

    for (final Point2D base : asteroids) {
      HashMap<Double, Set<Point2D>> visible = new HashMap<>();
      for (final Point2D asteroid : asteroids) {
        double angle = getAngle(base, asteroid);
        if (!visible.containsKey(angle)) {
          visible.put(angle, new HashSet<>());
        }

        visible.get(angle).add(asteroid);
      }
      if (visible.size() > maxVisible) {
        maxVisible = visible.size();
      }
    }

    return String.valueOf(maxVisible);
  }

  @Override
  public String partTwo() throws IOException {
    return "-";
  }

  private ArrayList<Point2D> getAsteroids(final String inputMap) {
    AtomicInteger currY = new AtomicInteger();
    ArrayList<Point2D> asteroids = new ArrayList<>();

    inputMap
        .lines()
        .forEach(
            line -> {
              String[] objects = line.split("");
              for (int x = 0; x < objects.length; x++) {
                if (objects[x].equals("#")) {
                  asteroids.add(Point2D.on(x, currY.get()));
                }
              }

              currY.incrementAndGet();
            });

    return asteroids;
  }

  private double getAngle(final Point2D pointA, final Point2D pointB) {
    double angle =
        Math.atan2(pointB.getY() - pointA.getY(), pointB.getX() - pointA.getX()) * 180 / Math.PI;
    if (angle < 0) {
      return 360 - angle;
    }

    return angle;
  }
}
