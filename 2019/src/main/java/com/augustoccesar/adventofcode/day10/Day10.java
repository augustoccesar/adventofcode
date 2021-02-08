package com.augustoccesar.adventofcode.day10;

import com.augustoccesar.adventofcode.Task;
import com.augustoccesar.adventofcode.utils.Point2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;

public class Day10 extends Task {
  @Override
  public String partOne() throws IOException {
    return "-";
  }

  @Override
  public String partTwo() throws IOException {
    return "-";
  }

  private ArrayList<Point2D> getAsteroids(final String map) {
    AtomicInteger currY = new AtomicInteger();
    ArrayList<Point2D> asteroids = new ArrayList<>();

    map.lines()
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
}
