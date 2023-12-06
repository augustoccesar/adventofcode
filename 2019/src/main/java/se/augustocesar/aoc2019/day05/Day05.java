package se.augustocesar.aoc2019.day05;

import se.augustocesar.aoc2019.shared.intcomputer.IntComputer;
import se.augustocesar.aoc2019.task.RunnableTask;
import se.augustocesar.aoc2019.task.Task;

@RunnableTask(day = 5)
public class Day05 extends Task {

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
