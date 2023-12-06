package se.augustocesar.aoc2019.task;

import static org.reflections.scanners.Scanners.SubTypes;
import static org.reflections.scanners.Scanners.TypesAnnotated;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Set;
import org.reflections.Reflections;

public class TaskLoader {

  private final String packageName;

  public TaskLoader(String packageName) {
    this.packageName = packageName;
  }

  public HashMap<Integer, Task> registeredTasks() {
    Reflections reflections = new Reflections(this.packageName);

    Set<Class<?>> annotated = reflections.get(
        SubTypes.of(
            TypesAnnotated.with(RunnableTask.class)
        ).asClass()
    );
    HashMap<Integer, Task> taskMap = new HashMap<>();

    for (Class<?> cls : annotated) {
      try {
        Task task = (Task) cls.getConstructor().newInstance();
        int day = cls.getAnnotation(RunnableTask.class).day();
        if (taskMap.containsKey(day)) {
          throw new RuntimeException(
              String.format("Task with day %s defined more than once by RunnableTask", day)
          );
        }

        taskMap.put(day, task);
      } catch (NoSuchMethodException | InvocationTargetException | InstantiationException |
               IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    }

    return taskMap;
  }
}
