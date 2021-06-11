package com.augustoccesar.adventofcode.day09;

import com.augustoccesar.adventofcode.Task;
import com.augustoccesar.adventofcode.shared.intcomputer.IntComputer;
import java.io.IOException;

public class Day09 extends Task {

  @Override
  public String partOne() throws IOException {
    String program = this.readInput();
    IntComputer computer = IntComputer.load(program);
    computer.inputWrite(1);
    computer.runUntilHalted();

    return String.valueOf(computer.outputRead());
  }

  @Override
  public String partTwo() throws IOException {
    String program = this.readInput();
    IntComputer computer = IntComputer.load(program);
    computer.inputWrite(2);
    computer.runUntilHalted();

    return String.valueOf(computer.outputRead());
  }
}
