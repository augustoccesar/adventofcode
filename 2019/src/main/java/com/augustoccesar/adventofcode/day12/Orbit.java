package com.augustoccesar.adventofcode.day12;

import java.util.List;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class Orbit {

  @Getter
  private int ticks = 1;

  @Getter
  private final List<Moon> moons;

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

  public int energy() {
    return this.moons.stream()
        .map(Moon::totalEnergy)
        .reduce(Integer::sum)
        .orElseThrow(() -> new RuntimeException("Failed to sum total energy of the moons"));
  }
}
