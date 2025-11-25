package com.augustoccesar.aocjava.y2019;

import com.augustoccesar.aocjava.Day;
import com.augustoccesar.aocjava.RunnableDay;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@RunnableDay(year = 2019, day = 12)
public class Day12 extends Day {

  private static final Pattern COORD_PATTERN =
      Pattern.compile("<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>");

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

@AllArgsConstructor(access = AccessLevel.PRIVATE)
class Moon {

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

@AllArgsConstructor(access = AccessLevel.PRIVATE)
class Orbit {

  @Getter private int ticks = 1;

  @Getter private final List<Moon> moons;

  public Orbit(List<Moon> moons) {
    this.moons = moons;
  }

  public static Orbit with(final List<Moon> moons) {
    return new Orbit(moons);
  }

  public void tick() {
    // TODO: Think of a way to avoid these two loops?
    for (final Moon moon : this.moons) {
      moon.applyGravity(this.moons);
    }

    for (final Moon moon : this.moons) {
      moon.applyVelocity();
    }

    ticks++;
  }

  @SuppressWarnings("null")
  public int energy() {
    return this.moons.stream()
        .map(Moon::totalEnergy)
        .reduce(Integer::sum)
        .orElseThrow(() -> new RuntimeException("Failed to sum total energy of the moons"));
  }
}

@AllArgsConstructor
class Coord3D {

  @Getter @Setter private int x;

  @Getter @Setter private int y;

  @Getter @Setter private int z;

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
