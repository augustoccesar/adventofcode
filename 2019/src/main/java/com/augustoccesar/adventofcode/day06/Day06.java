package com.augustoccesar.adventofcode.day06;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map.Entry;

public class Day06 extends Task {
  @Override
  public String partOne() throws IOException {
    HashMap<String, Planet> planets = new HashMap<>();
    final String input = this.readInput();

    input
        .lines()
        .forEach(
            line -> {
              final String[] planetNames = line.split("\\)");
              final Planet basePlanet;

              if (planets.get(planetNames[0]) != null) {
                basePlanet = planets.get(planetNames[0]);
              } else {
                basePlanet = Planet.builder().name(planetNames[0]).build();
                planets.putIfAbsent(basePlanet.getName(), basePlanet);
              }

              if (planets.get(planetNames[1]) != null) {
                planets.get(planetNames[1]).setOrbit(basePlanet);
              } else {
                planets.put(
                    planetNames[1],
                    Planet.builder().name(planetNames[1]).orbit(basePlanet).build());
              }
            });

    int totalOrbits = 0;
    for (Entry<String, Planet> entry : planets.entrySet()) {
      totalOrbits += countOrbits(entry.getValue(), 0);
    }

    return String.valueOf(totalOrbits);
  }

  @Override
  public String partTwo() throws IOException {
    return null;
  }

  private int countOrbits(final Planet planet, int result) {
    if (planet.getOrbit() != null) {
      return countOrbits(planet.getOrbit(), result + 1);
    }

    return result;
  }
}
