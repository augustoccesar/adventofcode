package com.augustoccesar.adventofcode.day05;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Day05 extends Task {

  @Override
  public String partOne() throws IOException {
    String program = this.readInput().strip();
    IntComputer computer = IntComputer.load(program);
    computer.addInput(1);

    while (!computer.isHalted()) {
      computer.run();
    }

    return String.valueOf(computer.lastOutput());
  }

  @Override
  public String partTwo() throws IOException {
    String program = this.readInput().strip();
    IntComputer computer = IntComputer.load(program);
    computer.addInput(5);

    while (!computer.isHalted()) {
      computer.run();
    }

    return String.valueOf(computer.lastOutput());
  }
}
