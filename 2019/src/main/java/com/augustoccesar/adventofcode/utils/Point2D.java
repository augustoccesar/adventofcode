package com.augustoccesar.adventofcode.utils;

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
   * </p>
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
}
