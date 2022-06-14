package com.augustoccesar.adventofcode.day02;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Day02 extends Task {

  @Override
  public String partOne() throws IOException {
    int output = generateOutput(12, 2);

    return String.valueOf(output);
  }

  @Override
  public String partTwo() throws IOException {
    boolean found = false;
    int expected = 19690720;
    int noun = -1;
    int verb = -1;

    for (int i = 99; i >= 0 && !found; i--) {
      for (int j = 99; j >= 0 && !found; j--) {
        int res = generateOutput(i, j);
        if (res == expected) {
          noun = i;
          verb = j;
          found = true;
        }
      }
    }

    int result = 100 * noun + verb;
    return String.valueOf(result);
  }

  public int generateOutput(int noun, int verb) throws IOException {
    String input = readInput().trim();
    List<Integer> inputArr =
        Stream.of(input.split(",")).map(Integer::parseInt).collect(Collectors.toList());

    inputArr.set(1, noun);
    inputArr.set(2, verb);

    int opcode = -1;
    int i = 0;
    while (opcode != 99) {
      opcode = inputArr.get(i);

      int targetIdx = inputArr.get(i + 3);
      int firstValue = inputArr.get(inputArr.get(i + 1));
      int secondValue = inputArr.get(inputArr.get(i + 2));

      if (opcode == 1) {
        inputArr.set(targetIdx, firstValue + secondValue);
      }

      if (opcode == 2) {
        inputArr.set(targetIdx, firstValue * secondValue);
      }

      i += 4;
    }

    return inputArr.get(0);
  }
}
