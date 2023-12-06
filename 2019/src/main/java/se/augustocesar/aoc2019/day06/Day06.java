package se.augustocesar.aoc2019.day06;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;
import se.augustocesar.aoc2019.task.RunnableTask;
import se.augustocesar.aoc2019.task.Task;

@RunnableTask(day = 6)
public class Day06 extends Task {

  @Override
  public String partOne() {
    HashMap<String, Planet> planets = buildPlanetsMap(this.readInputLines());

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
  public String partTwo() {
    HashMap<String, Planet> planets = buildPlanetsMap(this.readInputLines());

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
            stepsSan.stream().takeWhile(item -> !item.equals(intersectPlanet.get().getName())),
            stepsYou.stream().takeWhile(item -> !item.equals(intersectPlanet.get().getName()))
        ).count();

    return String.valueOf(res);
  }

  private HashMap<String, Planet> buildPlanetsMap(final List<String> input) {
    HashMap<String, Planet> planets = new HashMap<>();

    input
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
