package com.augustoccesar.adventofcode.day05;

import com.augustoccesar.adventofcode.Task;
import com.augustoccesar.adventofcode.shared.intcomputer.IntComputer;
import java.io.IOException;

public class Day05 extends Task {

  @Override
  public String partOne() throws IOException {
    String program = this.readInput().strip();
    IntComputer computer = IntComputer.load(program);
    computer.getInputSource().write(1);
    computer.runUntilHalted();

    return String.valueOf(computer.outputRead());
  }

  @Override
  public String partTwo() throws IOException {
    String program = this.readInput().strip();
    IntComputer computer = IntComputer.load(program);
    computer.getInputSource().write(5);
    computer.runUntilHalted();

    return String.valueOf(computer.outputRead());
  }
}
