package com.augustoccesar.adventofcode.day07;

import com.augustoccesar.adventofcode.Task;
import com.augustoccesar.adventofcode.day05.IntComputer;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

public class Day07 extends Task {

  @Override
  public String partOne() throws IOException {
    final String program = this.readInput().strip();
    final int[] phaseList = new int[]{0, 1, 2, 3, 4};
    final AtomicInteger maxOut = new AtomicInteger(Integer.MIN_VALUE);

    execPermute(
        phaseList,
        0,
        phaseSettings -> {
          int lastOutput = 0;
          for (final int phase : phaseSettings) {
            IntComputer computer = IntComputer.load(program);
            computer.addInput(phase, lastOutput);
            while (!computer.isHalted()) {
              computer.run();
            }

            lastOutput = computer.lastOutput();
          }

          if (lastOutput > maxOut.get()) {
            maxOut.set(lastOutput);
          }
        });

    return String.valueOf(maxOut.get());
  }

  @Override
  public String partTwo() throws IOException {
    final String program = this.readInput().strip();
    final int[] phaseList = new int[]{5, 6, 7, 8, 9};
    final AtomicInteger maxOut = new AtomicInteger(Integer.MIN_VALUE);

    execPermute(
        phaseList,
        0,
        phaseSettings -> {
          Circuit circuit = new Circuit(program, phaseSettings);
          int circuitRunResult = circuit.run();
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
