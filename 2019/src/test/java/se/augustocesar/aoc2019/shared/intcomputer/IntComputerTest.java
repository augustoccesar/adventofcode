package se.augustocesar.aoc2019.shared.intcomputer;


import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

public class IntComputerTest {

  @Test
  public void constructWithFrom() {
    final String program = "1,0,0,3,99";
    final IntComputer intComputer = IntComputer.load(program);

    assertEquals(5, intComputer.getMemory().getUnderlying().size());

    assertEquals(1, intComputer.getMemory().read(0L));
    assertEquals(0, intComputer.getMemory().read(1L));
    assertEquals(0, intComputer.getMemory().read(2L));
    assertEquals(3, intComputer.getMemory().read(3L));
    assertEquals(99, intComputer.getMemory().read(4L));
  }

  @Test
  public void day2_example1() {
    final String program = "1,9,10,3,2,3,11,0,99,30,40,50";
    final HashMap<Long, Long> expectedMemory = generateMemoryFromArray(
        new long[]{3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50}
    );

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.runUntilHalted();

    assertEquals(expectedMemory, intComputer.getMemory().getUnderlying());
  }

  @Test
  public void day2_example2() {
    final String program = "1,0,0,0,99";
    final HashMap<Long, Long> expectedMemory = generateMemoryFromArray(
        new long[]{2, 0, 0, 0, 99}
    );

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.runUntilHalted();

    assertEquals(expectedMemory, intComputer.getMemory().getUnderlying());
  }

  @Test
  public void day2_example3() {
    final String program = "2,3,0,3,99";
    final HashMap<Long, Long> expectedMemory = generateMemoryFromArray(
        new long[]{2, 3, 0, 6, 99}
    );

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.runUntilHalted();

    assertEquals(expectedMemory, intComputer.getMemory().getUnderlying());
  }

  @Test
  public void day2_example4() {
    final String program = "2,4,4,5,99,0";
    final HashMap<Long, Long> expectedMemory = generateMemoryFromArray(
        new long[]{2, 4, 4, 5, 99, 9801}
    );

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.runUntilHalted();

    assertEquals(expectedMemory, intComputer.getMemory().getUnderlying());
  }

  @Test
  public void day2_example5() {
    final String program = "1,1,1,4,99,5,6,0,99";
    final HashMap<Long, Long> expectedMemory = generateMemoryFromArray(
        new long[]{30, 1, 1, 4, 2, 5, 6, 0, 99}
    );

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.runUntilHalted();

    assertEquals(expectedMemory, intComputer.getMemory().getUnderlying());
  }

  @Test
  public void day2_Part1() {
    final long expectedAt0 = 3409710; // Well, I guess I have to live with this spoiler
    final String program =
        "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,9,23,1,5,23,27,1,27,9,31,1,6,31,35,2,35,"
            + "9,39,1,39,6,43,2,9,43,47,1,47,6,51,2,51,9,55,1,5,55,59,2,59,6,63,1,9,63,67,1,67,10,"
            + "71,1,71,13,75,2,13,75,79,1,6,79,83,2,9,83,87,1,87,6,91,2,10,91,95,2,13,95,99,1,9,"
            + "99,103,1,5,103,107,2,9,107,111,1,111,5,115,1,115,5,119,1,10,119,123,1,13,123,127,"
            + "1,2,127,131,1,131,13,0,99,2,14,0,0";

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.runUntilHalted();

    final long valAt0 = intComputer.getMemory().read(0);

    assertEquals(expectedAt0, valAt0);
  }

  @Test
  public void day5_part1_example1() {
    final String program = "3,0,4,0,99";
    final long input = 1;
    final long expectedOutput = 1;

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.getInputSource().write(input);
    intComputer.runUntilHalted();

    final long output = intComputer.outputRead();

    assertEquals(expectedOutput, output);
  }

  @Test
  public void day5_part1_example2() {
    final String program = "1002,4,3,4,33";
    final HashMap<Long, Long> expectedMemory = generateMemoryFromArray(
        new long[]{1002, 4, 3, 4, 99}
    );

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.runUntilHalted();

    assertEquals(expectedMemory, intComputer.getMemory().getUnderlying());
  }

  @Test
  public void day5_part1() {
    final String program = "3,225,1,225,6,6,1100,1,238,225,104,0,2,136,183,224,101,-5304,224,"
        + "224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,1101,72,47,225,1101,59,55,225,"
        + "1101,46,75,225,1101,49,15,224,101,-64,224,224,4,224,1002,223,8,223,1001,224,5,224,1,"
        + "224,223,223,102,9,210,224,1001,224,-270,224,4,224,1002,223,8,223,1001,224,2,224,1,223,"
        + "224,223,101,14,35,224,101,-86,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,"
        + "1102,40,74,224,1001,224,-2960,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,"
        + "1101,10,78,225,1001,39,90,224,1001,224,-149,224,4,224,102,8,223,223,1001,224,4,224,1,"
        + "223,224,223,1002,217,50,224,1001,224,-1650,224,4,224,1002,223,8,223,1001,224,7,224,1,"
        + "224,223,223,1102,68,8,225,1,43,214,224,1001,224,-126,224,4,224,102,8,223,223,101,3,224,"
        + "224,1,224,223,223,1102,88,30,225,1102,18,80,225,1102,33,28,225,4,223,99,0,0,0,677,0,0,"
        + "0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,"
        + "1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,"
        + "99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,"
        + "300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,677,677,224,102,2,"
        + "223,223,1005,224,329,1001,223,1,223,1107,677,226,224,102,2,223,223,1006,224,344,1001,"
        + "223,1,223,108,226,226,224,102,2,223,223,1005,224,359,1001,223,1,223,1108,677,226,224,"
        + "102,2,223,223,1006,224,374,101,1,223,223,108,677,226,224,102,2,223,223,1006,224,389,"
        + "1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,404,1001,223,1,223,8,226,226,"
        + "224,102,2,223,223,1006,224,419,101,1,223,223,1107,677,677,224,102,2,223,223,1006,224,"
        + "434,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,449,101,1,223,223,7,677,"
        + "677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,226,677,224,1002,223,2,223,"
        + "1005,224,479,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,494,101,1,223,223,"
        + "7,226,677,224,102,2,223,223,1005,224,509,101,1,223,223,1008,677,226,224,102,2,223,"
        + "223,1006,224,524,101,1,223,223,8,226,677,224,1002,223,2,223,1006,224,539,1001,223,1,"
        + "223,1007,677,677,224,102,2,223,223,1005,224,554,101,1,223,223,107,226,677,224,1002,"
        + "223,2,223,1005,224,569,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,584,"
        + "1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,599,101,1,223,223,1008,677,"
        + "677,224,102,2,223,223,1005,224,614,101,1,223,223,7,677,226,224,1002,223,2,223,1005,"
        + "224,629,1001,223,1,223,107,677,677,224,1002,223,2,223,1006,224,644,101,1,223,223,"
        + "1007,226,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,1007,226,226,224,102,2,"
        + "223,223,1005,224,674,101,1,223,223,4,223,99,226";
    final long input = 1;
    final long expectedOutput = 13978427;

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.getInputSource().write(input);
    intComputer.runUntilHalted();

    assertEquals(expectedOutput, intComputer.outputRead());
  }

  @Test
  public void day5_part2_example1() {
    // Using position mode, consider whether the input is equal to 8
    final String program = "3,9,8,9,10,9,4,9,99,-1,8";
    final long input1 = 8;
    final long expected1 = 1;
    final long input2 = 9;
    final long expected2 = 0;

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.getInputSource().write(input1);
    intComputer.runUntilHalted();

    assertEquals(expected1, intComputer.outputRead());

    intComputer.reset();
    intComputer.getInputSource().write(input2);
    intComputer.runUntilHalted();

    assertEquals(expected2, intComputer.outputRead());
  }

  @Test
  public void day5_part2_example2() {
    // Using position mode, consider whether the input is less than 8
    final String program = "3,9,7,9,10,9,4,9,99,-1,8";
    final long input1 = 7;
    final long expected1 = 1;
    final long input2 = 9;
    final long expected2 = 0;

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.getInputSource().write(input1);
    intComputer.runUntilHalted();

    assertEquals(expected1, intComputer.outputRead());

    intComputer.reset();
    intComputer.getInputSource().write(input2);
    intComputer.runUntilHalted();

    assertEquals(expected2, intComputer.outputRead());
  }

  @Test
  public void day5_part2_example3() {
    // Using immediate mode, consider whether the input is equal to 8
    final String program = "3,3,1108,-1,8,3,4,3,99";
    final long input1 = 8;
    final long expected1 = 1;
    final long input2 = 9;
    final long expected2 = 0;

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.getInputSource().write(input1);
    intComputer.runUntilHalted();

    assertEquals(expected1, intComputer.outputRead());

    intComputer.reset();
    intComputer.getInputSource().write(input2);
    intComputer.runUntilHalted();

    assertEquals(expected2, intComputer.outputRead());
  }

  @Test
  public void day5_part2_example4() {
    // Using immediate mode, consider whether the input is less than 8
    final String program = "3,3,1107,-1,8,3,4,3,99";
    final long input1 = 7;
    final long expected1 = 1;
    final long input2 = 9;
    final long expected2 = 0;

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.getInputSource().write(input1);
    intComputer.runUntilHalted();

    assertEquals(expected1, intComputer.outputRead());

    intComputer.reset();
    intComputer.getInputSource().write(input2);
    intComputer.runUntilHalted();

    assertEquals(expected2, intComputer.outputRead());
  }

  @Test
  public void day5_part2_example5() {
    // Jump test, check if input is 0 using position mode
    final String program = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9";
    final long input1 = 0;
    final long expected1 = 0;
    final long input2 = 2;
    final long expected2 = 1;

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.getInputSource().write(input1);
    intComputer.runUntilHalted();

    assertEquals(expected1, intComputer.outputRead());

    intComputer.reset();
    intComputer.getInputSource().write(input2);
    intComputer.runUntilHalted();

    assertEquals(expected2, intComputer.outputRead());
  }

  @Test
  public void day5_part2_example6() {
    // Jump test, check if input is 0 using immediate mode
    final String program = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1";
    final long input1 = 0;
    final long expected1 = 0;
    final long input2 = 2;
    final long expected2 = 1;

    final IntComputer intComputer = IntComputer.load(program);
    intComputer.getInputSource().write(input1);
    intComputer.runUntilHalted();

    assertEquals(expected1, intComputer.outputRead());

    intComputer.reset();
    intComputer.getInputSource().write(input2);
    intComputer.runUntilHalted();

    assertEquals(expected2, intComputer.outputRead());
  }

  @Test
  public void day5_part2_example7() {
    // The program will then output 999 if the input value is below 8, output 1000 if the input
    // value is equal to 8, or output 1001 if the input value is greater than 8.
    String program = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,"
        + "0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99";

    final long[] inputs = new long[]{5, 8, 10};
    final long[] expectedOutputs = new long[]{999, 1000, 1001};

    final IntComputer intComputer = IntComputer.load(program);
    for (int i = 0; i < inputs.length; i++) {
      intComputer.reset();
      intComputer.getInputSource().write(inputs[i]);
      intComputer.runUntilHalted();

      assertEquals(expectedOutputs[i], intComputer.outputRead());
    }
  }

  @Test
  public void day7_part1_example1() {
    final String program = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
    final int[] phaseSequence = new int[]{4, 3, 2, 1, 0};

    final IntComputer computerA = IntComputer.load(
        program,
        MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST)
    );
    computerA.getInputSource().write(phaseSequence[0]);
    computerA.getInputSource().write(0);
    computerA.runUntilHalted();

    final IntComputer computerB = IntComputer.load(
        program,
        MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST)
    );
    computerB.getInputSource().write(phaseSequence[1]);
    computerB.getInputSource().write(computerA.outputRead());
    computerB.runUntilHalted();

    final IntComputer computerC = IntComputer.load(
        program,
        MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST)
    );
    computerC.getInputSource().write(phaseSequence[2]);
    computerC.getInputSource().write(computerB.outputRead());
    computerC.runUntilHalted();

    final IntComputer computerD = IntComputer.load(
        program,
        MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST)
    );
    computerD.getInputSource().write(phaseSequence[3]);
    computerD.getInputSource().write(computerC.outputRead());
    computerD.runUntilHalted();

    final IntComputer computerE = IntComputer.load(
        program,
        MemoryInputSource.with(MemoryInputAccessMode.POOL_FIRST)
    );
    computerE.getInputSource().write(phaseSequence[4]);
    computerE.getInputSource().write(computerD.outputRead());
    computerE.runUntilHalted();

    assertEquals(43210, computerE.outputRead());
  }

  @Test
  public void day9_part1_example1() {
    // Takes no input and produces a copy of itself as output.
    String program = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";
    final IntComputer computer = IntComputer.load(program);
    computer.runUntilHalted();

    String result = computer.getOutput().stream()
        .map(String::valueOf)
        .collect(Collectors.joining(","));

    assertEquals(program, result);
  }

  @Test
  public void day9_part1_example2() {
    // Should output a 16-digit number.
    String program = "1102,34915192,34915192,7,4,7,99,0";
    final IntComputer computer = IntComputer.load(program);
    computer.runUntilHalted();

    long result = computer.outputRead();

    assertEquals(16, String.valueOf(result).length());
  }

  @Test
  public void day9_part1_example3() {
    // Should output the large number in the middle.
    String program = "104,1125899906842624,99";
    final IntComputer computer = IntComputer.load(program);
    computer.runUntilHalted();

    long result = computer.outputRead();

    assertEquals(1125899906842624L, result);
  }

  // ###############################################################################################
  // # Helper functions ############################################################################
  // ###############################################################################################


  private HashMap<Long, Long> generateMemoryFromArray(final long[] items) {
    final HashMap<Long, Long> memory = new HashMap<>();

    for (int i = 0; i < items.length; i++) {
      memory.put((long) i, items[i]);
    }

    return memory;
  }
}