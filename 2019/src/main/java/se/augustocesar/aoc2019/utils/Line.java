package se.augustocesar.aoc2019.utils;

import java.util.ArrayList;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class Line {
  private Point2D start;
  private Point2D end;

  public static Line between(final Point2D start, final Point2D end) {
    return new Line(start, end);
  }

  // https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
  public ArrayList<Point2D> points() {
    ArrayList<Point2D> points = new ArrayList<>();

    int newM = 2 * (end.getY() - start.getY());
    int slopeErrorNew = newM - (end.getX() - start.getX());

    for (int x = start.getX(), y = start.getY(); x <= end.getX(); x++) {
      points.add(Point2D.on(x, y));
      slopeErrorNew += newM;

      if (slopeErrorNew >= 0) {
        y++;
        slopeErrorNew -= 2 * (end.getX() - start.getX());
      }
    }
    return new ArrayList<>();
  }
}
