package se.augustocesar.aoc2019;

import java.util.HashMap;
import se.augustocesar.aoc2019.task.Task;
import se.augustocesar.aoc2019.task.TaskLoader;

public class Main {

  public static void main(String[] args) {
    if (args.length < 1) {
      throw new IllegalArgumentException("Invalid amount of args");
    }

    final int dayArg = Integer.parseInt(args[0]);
    final HashMap<Integer, Task> availableTasks = new TaskLoader("se.augustocesar.aoc2019")
        .registeredTasks();

    if (!availableTasks.containsKey(dayArg)) {
      System.err.println(String.format("Day %d not found", dayArg));
      return;
    }

    availableTasks.get(dayArg).run();
  }
}
