package com.augustoccesar.aocjava;

import static org.reflections.scanners.Scanners.SubTypes;
import static org.reflections.scanners.Scanners.TypesAnnotated;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Optional;
import java.util.Set;
import org.reflections.Reflections;

public class DayLoader {
  private HashMap<String, Day> dayMap;

  public static DayLoader load() {
    HashMap<String, Day> dayMap = new HashMap<>();
    var dayLoader = new DayLoader(dayMap);

    Reflections reflections = new Reflections("com.augustoccesar.aocjava");

    Set<Class<?>> annotated =
        reflections.get(SubTypes.of(TypesAnnotated.with(RunnableDay.class)).asClass());

    for (Class<?> cls : annotated) {
      Day dayInstance;
      try {
        dayInstance = (Day) cls.getConstructor().newInstance();

        if (dayLoader.getDay(dayInstance.getYear(), dayInstance.getDay()).isPresent()) {
          throw new RuntimeException(
              String.format(
                  "Task with day %s and year %s defined more than once by RunnableTask",
                  dayInstance.getDay(), dayInstance.getYear()));
        }

        dayMap.put(dayKey(dayInstance.getYear(), dayInstance.getDay()), dayInstance);
      } catch (InstantiationException
          | IllegalAccessException
          | IllegalArgumentException
          | InvocationTargetException
          | NoSuchMethodException e) {
        throw new RuntimeException(e);
      }
    }

    return new DayLoader(dayMap);
  }

  public Optional<Day> getDay(int year, int day) {
    return Optional.ofNullable(this.dayMap.get(dayKey(year, day)));
  }

  private DayLoader(HashMap<String, Day> dayMap) {
    this.dayMap = dayMap;
  }

  private static String dayKey(int year, int day) {
    return String.format("%s-%s", year, day);
  }
}
