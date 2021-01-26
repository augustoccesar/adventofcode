package com.augustoccesar.adventofcode.day05;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Day05 extends Task {
  @Override
  public String partOne() throws IOException {
    String program = this.readInput().strip();
    ArrayList<Integer> input = new ArrayList<>(List.of(1));
    int result = IntComputer.load(program).run(input);

    return String.valueOf(result);
  }

  @Override
  public String partTwo() throws IOException {
    String program = this.readInput().strip();
    ArrayList<Integer> input = new ArrayList<>(List.of(5));
    int result = IntComputer.load(program).run(input);

    return String.valueOf(result);
  }
}
