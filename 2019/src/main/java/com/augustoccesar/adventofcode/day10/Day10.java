package com.augustoccesar.adventofcode.day10;

import com.augustoccesar.adventofcode.Task;
import com.augustoccesar.adventofcode.utils.ListUtil;
import com.augustoccesar.adventofcode.utils.Point2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class Day10 extends Task {

  @Override
  public String partOne() throws IOException {
    ArrayList<Point2D> asteroids = this.getAsteroids(this.readInput());
    int maxVisible = 0;

    for (final Point2D base : asteroids) {
      HashMap<Double, LinkedList<Point2D>> visible = buildRadar(base, asteroids, false);
      if (visible.size() > maxVisible) {
        maxVisible = visible.size();
      }
    }

    return String.valueOf(maxVisible);
  }

  @Override
  public String partTwo() throws IOException {
    final Point2D base = Point2D.on(22, 28);
    final ArrayList<Point2D> asteroids = this.getAsteroids(this.readInput());

    HashMap<Double, LinkedList<Point2D>> radar = buildRadar(base, asteroids, true);

    List<Double> angles = new ArrayList<>(radar.keySet());
    Collections.sort(angles);
    int startIdx = -1;
    for (int i = 0; startIdx == -1; i++) {
      if (angles.get(i) >= 270) {
        startIdx = i;
      }
    }
    angles = ListUtil.shiftLeft(angles, startIdx);

    int vaporized = 0;
    Point2D lastVaporized = null;

    while (vaporized < 200) {
      for (double angle : angles) {
        if (radar.get(angle).size() == 0) {
          continue;
        }

        lastVaporized = radar.get(angle).poll();
        vaporized++;
        if (vaporized == 200) {
          break;
        }
      }
    }

    int result = (lastVaporized.getX() * 100) + lastVaporized.getY();

    return String.valueOf(result);
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

  private HashMap<Double, LinkedList<Point2D>> buildRadar(
      final Point2D base, final ArrayList<Point2D> asteroids, final boolean ordered) {

    if (ordered) {
      asteroids.sort(
          (a, b) -> {
            int distA = base.distance(a);
            int distB = base.distance(b);

            return Integer.compare(distA, distB);
          });
    }

    HashMap<Double, LinkedList<Point2D>> visible = new HashMap<>();
    for (final Point2D asteroid : asteroids) {
      double angle = base.angleBetween(asteroid);
      if (!visible.containsKey(angle)) {
        visible.put(angle, new LinkedList<>());
      }

      visible.get(angle).add(asteroid);
    }

    return visible;
  }
}
