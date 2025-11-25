package com.augustoccesar.aocjava.y2019;

import com.augustoccesar.aocjava.Day;
import com.augustoccesar.aocjava.RunnableDay;
import com.augustoccesar.aocjava.y2019.shared.intcomputer.IntComputer;

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
