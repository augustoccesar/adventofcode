package com.augustoccesar.adventofcode.day07;

import com.augustoccesar.adventofcode.day05.IntComputer;
import java.util.ArrayList;
import java.util.stream.IntStream;

public class Circuit {

  private final ArrayList<IntComputer> amplifiers;
  private int currentAmplifier = 0;

  public Circuit(final String program, final int[] phaseSettings) {
    this.amplifiers = new ArrayList<>();

    IntStream.range(0, phaseSettings.length)
        .forEach(idx -> {
          IntComputer amplifier = IntComputer.load(program);
          amplifier.addInput(phaseSettings[idx]);
          if (idx == 0) {
            amplifier.addInput(0);
          }

          this.amplifiers.add(amplifier);
        });
  }

  public long run() {
    while (true) {
      IntComputer currentAmplifier = this.amplifiers.get(this.currentAmplifier);
      currentAmplifier.run();

      if (currentAmplifier.isHalted()) {
        return this.amplifiers.get(this.amplifiers.size() - 1).lastOutput();
      }

      int nextIdx;
      if (this.currentAmplifier + 1 == this.amplifiers.size()) {
        nextIdx = 0;
      } else {
        nextIdx = this.currentAmplifier + 1;
      }

      IntComputer nextAmplifier = this.amplifiers.get(nextIdx);
      nextAmplifier.addInput(currentAmplifier.lastOutput());
      this.currentAmplifier = nextIdx;
    }
  }
}
