package se.augustocesar.aoc2019.day06;

import se.augustocesar.aoc2019.Task;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

public class Day06 extends Task {

  @Override
  public String partOne() throws IOException {
    final String input = this.readInput();
    HashMap<String, Planet> planets = buildPlanetsMap(input);

    int totalOrbits = 0;
    for (Entry<String, Planet> entry : planets.entrySet()) {
      Planet currPlanet = entry.getValue();

      int orbits = 0;
      while (currPlanet.getOrbit() != null) {
        orbits++;
        currPlanet = currPlanet.getOrbit();
      }

      totalOrbits += orbits;
    }

    return String.valueOf(totalOrbits);
  }

  @Override
  public String partTwo() throws IOException {
    final String input = this.readInput();
    HashMap<String, Planet> planets = buildPlanetsMap(input);

    Planet currSan = planets.get("SAN").getOrbit();
    LinkedList<String> stepsSan = new LinkedList<>();

    Planet currYou = planets.get("YOU").getOrbit();
    LinkedList<String> stepsYou = new LinkedList<>();

    AtomicReference<Planet> intersectPlanet = new AtomicReference<>();

    while (currSan.getOrbit() != null) {
      stepsSan.add(currSan.getName());
      currSan = currSan.getOrbit();
    }

    while (currYou.getOrbit() != null) {
      stepsYou.add(currYou.getName());

      if (stepsSan.contains(currYou.getName()) && intersectPlanet.get() == null) {
        intersectPlanet.set(currYou);
      }

      currYou = currYou.getOrbit();
    }

    long res =
        Stream.concat(
                stepsSan.stream().takeWhile(item -> item != intersectPlanet.get().getName()),
                stepsYou.stream().takeWhile(item -> item != intersectPlanet.get().getName()))
            .count();

    return String.valueOf(res);
  }

  private HashMap<String, Planet> buildPlanetsMap(final String input) {
    HashMap<String, Planet> planets = new HashMap<>();

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

    return planets;
  }
}
