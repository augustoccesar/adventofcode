package se.augustocesar.aoc2019.day07;

import se.augustocesar.aoc2019.shared.intcomputer.MemoryInputAccessMode;
import se.augustocesar.aoc2019.shared.intcomputer.IntComputer;
import se.augustocesar.aoc2019.shared.intcomputer.MemoryInputSource;
import java.util.ArrayList;
import java.util.stream.IntStream;

public class Circuit {

  private final ArrayList<IntComputer> amplifiers;
  private int currentAmplifierIdx = 0;

  public Circuit(final String program, final int[] phaseSettings) {
    this.amplifiers = new ArrayList<>();

    IntStream.range(0, phaseSettings.length)
        .forEach(idx -> {
          IntComputer amplifier = IntComputer
              .load(program, MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST));
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
