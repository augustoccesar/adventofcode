package com.augustoccesar.adventofcode.day09;

import com.augustoccesar.adventofcode.Task;
import com.augustoccesar.adventofcode.day05.IntComputer;
import java.io.IOException;

public class Day09 extends Task {

  @Override
  public String partOne() throws IOException {
    String program = this.readInput();
    IntComputer computer = IntComputer.load(program);
    computer.addInput(1);

    while (!computer.isHalted()) {
      computer.run();
    }

    return String.valueOf(computer.lastOutput());
  }

  @Override
  public String partTwo() throws IOException {
    return "-";
  }
}
