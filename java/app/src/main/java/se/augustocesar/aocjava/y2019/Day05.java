package se.augustocesar.aocjava.y2019;

import se.augustocesar.aocjava.Day;
import se.augustocesar.aocjava.RunnableDay;
import se.augustocesar.aocjava.y2019.shared.intcomputer.IntComputer;

@RunnableDay(year = 2019, day = 5)
public class Day05 extends Day {
  @Override
  public String partOne() {
    String program = this.readInput().strip();
    IntComputer computer = IntComputer.load(program);
    computer.getInputSource().write(1);
    computer.runUntilHalted();

    return String.valueOf(computer.outputRead());
  }

  @Override
  public String partTwo() {
    String program = this.readInput().strip();
    IntComputer computer = IntComputer.load(program);
    computer.getInputSource().write(5);
    computer.runUntilHalted();

    return String.valueOf(computer.outputRead());
  }
}
