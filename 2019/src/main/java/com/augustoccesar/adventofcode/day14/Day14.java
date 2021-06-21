package com.augustoccesar.adventofcode.day14;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;

public class Day14 extends Task {

  @Override
  public String partOne() throws IOException {
    String input = this.readInput();
    NanoFactory nanoFactory = NanoFactory.fromSpec(input);
    int totalOfOres = nanoFactory.produce(1, "FUEL");

    return String.valueOf(totalOfOres);
  }

  @Override
  public String partTwo() throws IOException {
    return "-";
  }
}
