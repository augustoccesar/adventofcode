package se.augustocesar.aocjava.y2019;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.IntStream;
import se.augustocesar.aocjava.Day;
import se.augustocesar.aocjava.RunnableDay;
import se.augustocesar.aocjava.y2019.shared.intcomputer.IntComputer;
import se.augustocesar.aocjava.y2019.shared.intcomputer.MemoryInputAccessMode;
import se.augustocesar.aocjava.y2019.shared.intcomputer.MemoryInputSource;

@RunnableDay(year = 2019, day = 7)
public class Day07 extends Day {

  @Override
  public String partOne() {
    final String program = this.readInput().strip();
    final int[] phaseList = new int[] {0, 1, 2, 3, 4};
    final AtomicLong maxOut = new AtomicLong(Integer.MIN_VALUE);

    execPermute(
        phaseList,
        0,
        phaseSettings -> {
          long lastOutput = 0;
          for (final int phase : phaseSettings) {
            IntComputer computer =
                IntComputer.load(program, MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST));
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
    final int[] phaseList = new int[] {5, 6, 7, 8, 9};
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

class Circuit {
  private final ArrayList<IntComputer> amplifiers;
  private int currentAmplifierIdx = 0;

  public Circuit(final String program, final int[] phaseSettings) {
    this.amplifiers = new ArrayList<>();

    IntStream.range(0, phaseSettings.length)
        .forEach(
            idx -> {
              IntComputer amplifier =
                  IntComputer.load(
                      program, MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST));
              amplifier.getInputSource().write(phaseSettings[idx]);
              if (idx == 0) {
                amplifier.getInputSource().write(0);
              }

              this.amplifiers.add(amplifier);
            });
  }

  public long run() {
    while (true) {
      IntComputer currentAmplifier = this.amplifiers.get(this.currentAmplifierIdx);
      currentAmplifier.runUntilHaltedOrPaused();

      if (currentAmplifier.isHalted()) {
        return this.amplifiers.get(this.amplifiers.size() - 1).outputRead();
      }

      int nextIdx;
      if (this.currentAmplifierIdx + 1 == this.amplifiers.size()) {
        nextIdx = 0;
      } else {
        nextIdx = this.currentAmplifierIdx + 1;
      }

      IntComputer nextAmplifier = this.amplifiers.get(nextIdx);
      nextAmplifier.getInputSource().write(currentAmplifier.outputRead());
      this.currentAmplifierIdx = nextIdx;
    }
  }
}
