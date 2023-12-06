package se.augustocesar.aoc2019.day12;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@AllArgsConstructor
public class Coord3D {

  @Getter
  @Setter
  private int x;

  @Getter
  @Setter
  private int y;

  @Getter
  @Setter
  private int z;

  public static Coord3D of(int x, int y, int z) {
    return new Coord3D(x, y, z);
  }

  public static Coord3D ofDefault(int defaultValue) {
    return new Coord3D(defaultValue, defaultValue, defaultValue);
  }

  public void applyToX(final int x) {
    this.x += x;
  }

  public void applyToY(final int y) {
    this.y += y;
  }

  public void applyToZ(final int z) {
    this.z += z;
  }
}
