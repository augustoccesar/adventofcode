package se.augustocesar.aocjava.commands;

import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;
import picocli.CommandLine.Command;
import se.augustocesar.aocjava.DayRegistry;

@Command(name = "days")
public class Days implements Callable<Integer> {
  @Override
  public Integer call() throws Exception {
    var dayLoader = DayRegistry.load();

    Map<String, List<String>> yearDays =
        dayLoader.getDayMap().keySet().stream()
            .map(key -> key.split("-"))
            .collect(
                Collectors.groupingBy(
                    keys -> keys[0], Collectors.mapping(keys -> keys[1], Collectors.toList())));

    yearDays.forEach(
        (year, days) -> {
          System.out.printf("%s;%s\n", year, String.join(";", days));
        });

    return 0;
  }
}
