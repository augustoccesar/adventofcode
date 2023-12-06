package se.augustocesar.aoc2019.day07;

import se.augustocesar.aoc2019.shared.intcomputer.MemoryInputAccessMode;
import se.augustocesar.aoc2019.shared.intcomputer.IntComputer;
import se.augustocesar.aoc2019.shared.intcomputer.MemoryInputSource;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicLong;
import se.augustocesar.aoc2019.task.RunnableTask;
import se.augustocesar.aoc2019.task.Task;

@RunnableTask(day = 7)
public class Day07 extends Task {

  @Override
  public String partOne() {
    final String program = this.readInput().strip();
    final int[] phaseList = new int[]{0, 1, 2, 3, 4};
    final AtomicLong maxOut = new AtomicLong(Integer.MIN_VALUE);

    execPermute(
        phaseList,
        0,
        phaseSettings -> {
          long lastOutput = 0;
          for (final int phase : phaseSettings) {
            IntComputer computer = IntComputer.load(
                program,
                MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST)
            );
            computer.getInputSource().write(phase);
            computer.getInputSource().write(lastOutput);
            computer.runUntilHalted();

            lastOutput = computer.outputRead();
          }

          if (lastOutput > maxOut.get()) {
            maxOut.set(lastOutput);
          }
        });

    return String.valueOf(maxOut.get());
  }

  @Override
  public String partTwo() {
    final String program = this.readInput().strip();
    final int[] phaseList = new int[]{5, 6, 7, 8, 9};
    final AtomicLong maxOut = new AtomicLong(Long.MIN_VALUE);

    execPermute(
        phaseList,
        0,
        phaseSettings -> {
          Circuit circuit = new Circuit(program, phaseSettings);
          long circuitRunResult = circuit.run();
          if (circuitRunResult > maxOut.get()) {
            maxOut.set(circuitRunResult);
          }
        });

    return String.valueOf(maxOut.get());
  }

  private interface Executor {

    void execute(int[] list);
  }

  private static void execPermute(int[] list, int pivot, Executor ex) {
    if (pivot == list.length) {
      ex.execute(list);
      return;
    }

    for (int i = pivot; i < list.length; i++) {
      int[] permutation = list.clone();
      permutation[pivot] = list[i];
      permutation[i] = list[pivot];
      execPermute(permutation, pivot + 1, ex);
    }
  }
}
