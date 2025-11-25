package com.augustoccesar.aocjava.utils;

import java.util.ArrayList;
import java.util.List;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@EqualsAndHashCode
public class Point2D {

  private final int x;
  private final int y;

  /**
   * Creates a simple id representation of this {@link Point2D}.
   *
   * @return String id.
   */
  public String id() {
    return String.format("%d,%d", this.x, this.y);
  }

  /**
   * New {@link Point2D} builder.
   *
   * @param x Value for {@code x}.
   * @param y Value for {@code y}.
   * @return A new {@link Point2D}.
   */
  public static Point2D on(int x, int y) {
    return new Point2D(x, y);
  }

  /**
   * Create a new {@link Point2D} with moved {@code x} and {@code y} by {@code x} and {@code y}
   * amount.
   *
   * <p>Example:
   *
   * <pre>{@code
   * var point = Point2D.on(0,0);
   * var newPoint = point.move(1, -1);
   * assert newPoint.equals(Point2D.on(1, -1));
   * }</pre>
   *
   * @param x Change to apply to this {@link Point2D} {@code x} when creating the new one.
   * @param y Change to apply to this {@link Point2D} {@code y} when creating the new one.
   * @return A new {@link Point2D} with {@code x} and {@code y} moved.
   */
  public Point2D move(int x, int y) {
    return new Point2D(this.x + x, this.y + y);
  }

  /**
   * Create a new {@link Point2D} with an increased {@code x} by {@code amount}.
   *
   * @param amount Amount to increase the {@code x} by.
   * @return A new {@link Point2D} with {@code x} increased by {@code amount}.
   */
  public Point2D moveX(int amount) {
    return move(amount, 0);
  }

  /**
   * Create a new {@link Point2D} with an increased {@code y} by {@code amount}.
   *
   * @param amount Amount to increase the {@code y} by.
   * @return A new {@link Point2D} with {@code y} increased by {@code amount}.
   */
  public Point2D moveY(int amount) {
    return move(0, amount);
  }

  /**
   * Calculate the Taxicab geometry (Manhattan distance) from this {@link Point2D} to another one.
   *
   * @param other The other {@link Point2D} to calculate the distance to.
   * @return The distance between the two {@link Point2D}.
   */
  public int taxicab(final Point2D other) {
    return Math.abs(this.x - other.x) + Math.abs(this.y - other.y);
  }

  /**
   * Calculate the angle between this {@link Point2D} and another one.
   *
   * @param other The other {@link Point2D} to calculate the angle between.
   * @return The angle between the two {@link Point2D}.
   */
  public double angleBetween(final Point2D other) {
    double angle =
        Math.atan2(other.getY() - this.getY(), other.getX() - this.getX()) * 180 / Math.PI;

    if (angle < 0) {
      return 360 + angle;
    }

    if (angle >= 360) {
      return angle - 360 * (angle % 360);
    }

    return angle;
  }

  /**
   * Calculate the geometric distance between this {@link Point2D} and another one.
   *
   * @param other The other {@link Point2D} to calculate the distance to.
   * @return The distance between the two {@link Point2D}.
   */
  public int distance(final Point2D other) {
    int x = Math.abs(other.getX() - this.getX());
    double a = Math.pow(x, 2);

    int y = Math.abs(other.getY() - this.getY());
    double b = Math.pow(y, 2);

    double res = Math.sqrt(a + b);

    return (int) res;
  }

  /**
   * Find the min and max values for X and Y in a list of {@link Point2D}.
   *
   * @param points List of {@link Point2D} to find min and maxes.
   * @return A {@link Pair} containing two other {@link Point2D}. First one with min and max for X,
   *     and second one containing min and max for Y.
   */
  public static Pair<MinMax<Integer>, MinMax<Integer>> minMaxes(List<Point2D> points) {
    Pair<List<Integer>, List<Integer>> xsYs = splitXsYs(points);
    List<Integer> xs = xsYs.getLeft();
    List<Integer> ys = xsYs.getRight();

    MinMax<Integer> minMaxX = MinMax.from(xs.toArray(new Integer[] {}));
    MinMax<Integer> minMaxY = MinMax.from(ys.toArray(new Integer[] {}));

    return Pair.of(minMaxX, minMaxY);
  }

  /**
   * Transforms a list of {@link Point2D} into a {@link Pair} of {@link MinMax} for each of the
   * axis.
   *
   * @param points List of {@link Point2D} to be split.
   * @return {@link Pair} of {@link MinMax} for each of the axis.
   */
  public static Pair<List<Integer>, List<Integer>> splitXsYs(final List<Point2D> points) {
    List<Integer> xs = new ArrayList<>(points.size());
    List<Integer> ys = new ArrayList<>(points.size());

    for (final Point2D point2D : points) {
      xs.add(point2D.getX());
      ys.add(point2D.getY());
    }

    return Pair.of(xs, ys);
  }
}
