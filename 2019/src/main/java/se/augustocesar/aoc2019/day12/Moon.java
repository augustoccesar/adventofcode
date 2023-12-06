package se.augustocesar.aoc2019.day12;

import java.util.List;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class Moon {

  private final int id;
  private final Coord3D position;
  private final Coord3D velocity;

  public static Moon of(final int id, final Coord3D position) {
    return new Moon(id, position, Coord3D.ofDefault(0));
  }

  public static Moon of(final int id, final int x, final int y, final int z) {
    return Moon.of(id, Coord3D.of(x, y, z));
  }

  public void applyGravity(final List<Moon> moons) {
    for (final Moon moon : moons) {
      if (moon.id == this.id) {
        continue;
      }

      if (this.position.getX() < moon.position.getX()) {
        this.velocity.applyToX(1);
      } else if (this.position.getX() > moon.position.getX()) {
        this.velocity.applyToX(-1);
      }

      if (this.position.getY() < moon.position.getY()) {
        this.velocity.applyToY(1);
      } else if (this.position.getY() > moon.position.getY()) {
        this.velocity.applyToY(-1);
      }

      if (this.position.getZ() < moon.position.getZ()) {
        this.velocity.applyToZ(1);
      } else if (this.position.getZ() > moon.position.getZ()) {
        this.velocity.applyToZ(-1);
      }
    }
  }

  public void applyVelocity() {
    this.position.applyToX(this.velocity.getX());
    this.position.applyToY(this.velocity.getY());
    this.position.applyToZ(this.velocity.getZ());
  }

  public int potentialEnergy() {
    return Math.abs(this.position.getX())
        + Math.abs(this.position.getY())
        + Math.abs(this.position.getZ());
  }

  public int kineticEnergy() {
    return Math.abs(this.velocity.getX())
        + Math.abs(this.velocity.getY())
        + Math.abs(this.velocity.getZ());
  }

  public int totalEnergy() {
    return potentialEnergy() * kineticEnergy();
  }
}
