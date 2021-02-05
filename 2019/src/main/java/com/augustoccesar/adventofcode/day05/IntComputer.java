package com.augustoccesar.adventofcode.day05;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class IntComputer {

  private long relativeBase = 0L;
  private long cursor = 0L;

  @Getter private boolean halted;
  private boolean paused;
  private final HashMap<Long, Long> memory;
  private final LinkedList<Long> input;
  private final LinkedList<Long> output;

  private IntComputer(ArrayList<Long> memory) {
    this.halted = false;
    this.paused = false;
    this.memory = new HashMap<>();
    this.input = new LinkedList<>();
    this.output = new LinkedList<>();

    for (Long i = 0L; i < memory.size(); i++) {
      this.memory.put(i, memory.get(i.intValue()));
    }
  }

  public static IntComputer load(final String program) {
    ArrayList<Long> memory =
        Stream.of(program.split(","))
            .map(Long::parseLong)
            .collect(Collectors.toCollection(ArrayList::new));

    return new IntComputer(memory);
  }

  public void addInput(long... values) {
    Arrays.stream(values).forEach(this.input::offer);
  }

  public Long lastOutput() {
    return this.output.peekLast();
  }

  public void apply(Instruction instruction) {
    if (!instruction.isValid()) {
      throw new RuntimeException(String.format("Applying invalid instruction: %s", this));
    }

    List<Long> paramValues =
        instruction.getParameters().stream()
            .map(
                item -> {
                  if (item.isTarget) {
                    if (item.getMode() == ParameterMode.POSITION) {
                      return item.getValue();
                    } else if (item.getMode() == ParameterMode.RELATIVE) {
                      return item.getValue() + this.relativeBase;
                    }
                  }

                  if (item.getMode() == ParameterMode.POSITION) {
                    final long key = item.getValue();
                    if (!this.memory.containsKey(key)) {
                      this.memory.put(key, 0L);
                    }

                    return memory.get(key);
                  } else if (item.getMode() == ParameterMode.IMMEDIATE) {
                    return item.getValue();
                  } else if (item.getMode() == ParameterMode.RELATIVE) {
                    final long key = item.getValue() + this.relativeBase;
                    if (!this.memory.containsKey(key)) {
                      this.memory.put(key, 0L);
                    }

                    return memory.get(key);
                  } else {
                    throw new RuntimeException(
                        String.format("Invalid mode for parameter: %s", item.getMode()));
                  }
                })
            .collect(Collectors.toList());
    int lastIndex = paramValues.size() - 1;

    boolean cursorJumped = false;
    switch (instruction.getOperation()) {
      case READ -> {
        final long key = paramValues.get(0);
        Long currInput = input.pollFirst();
        if (currInput == null) {
          // It shouldn't reach here
          throw new RuntimeException("Unexpected empty input list");
        }
        this.memory.put(key, currInput);
      }
      case WRITE -> {
        final long key = paramValues.get(0);
        this.output.offer(this.memory.get(key));
        this.paused = true;
      }
      case SUM -> memory.put(
          paramValues.get(lastIndex),
          paramValues.stream().limit(lastIndex).reduce(0L, Long::sum));
      case MULTIPLY -> memory.put(
          paramValues.get(lastIndex),
          paramValues.stream().limit(lastIndex).reduce(1L, Math::multiplyExact));
      case JUMP_IF_TRUE -> {
        if (paramValues.get(0) != 0) {
          this.cursor = paramValues.get(1);
          cursorJumped = true;
        }
      }
      case JUMP_IF_FALSE -> {
        if (paramValues.get(0) == 0) {
          this.cursor = paramValues.get(1);
          cursorJumped = true;
        }
      }
      case LESS_THAN -> {
        if (paramValues.get(0) < paramValues.get(1)) {
          memory.put(paramValues.get(2), 1L);
        } else {
          memory.put(paramValues.get(2), 0L);
        }
      }
      case EQUALS -> {
        if (paramValues.get(0).equals(paramValues.get(1))) {
          memory.put(paramValues.get(2), 1L);
        } else {
          memory.put(paramValues.get(2), 0L);
        }
      }
      case ADJUST_RELATIVE_BASE -> {
        this.relativeBase += paramValues.get(0);
      }
      case EXIT -> {
        this.halted = true;
      }
      default -> {
        throw new RuntimeException("Trying to apply invalid operation.");
      }
    }

    if (!cursorJumped) {
      this.cursor += instruction.getOperation().getParamSize() + 1;
    }
  }

  public void run() {
    this.paused = false;
    while (!this.paused && !this.halted) {
      Operation op = Operation.from(this.memory.get(this.cursor));

      ArrayList<Long> rawSubInstruction = new ArrayList<>();
      for (long i = this.cursor; i < this.cursor + op.getParamSize() + 1; i++) {
        rawSubInstruction.add(this.memory.get(i));
      }
      Instruction instruction = Instruction.from(rawSubInstruction);

      this.apply(instruction);
    }
  }

  @AllArgsConstructor(access = AccessLevel.PRIVATE)
  @Getter
  static class Instruction {
    private final Operation operation;
    private final List<Parameter> parameters;

    public static Instruction from(final List<Long> rawInstruction) {
      Operation op = Operation.from(rawInstruction.get(0));

      if (rawInstruction.size() == 1) { // Doesn't contain ant parameters
        return new Instruction(op, Collections.emptyList());
      }

      String opString = String.valueOf(rawInstruction.get(0));
      final Operation operation = Operation.from(opString.charAt(opString.length() - 1));
      final int maxOpStringSize = 2 + operation.paramSize;
      if (opString.length() < maxOpStringSize) {
        int diff = maxOpStringSize - opString.length();
        opString = "0".repeat(diff) + opString;
      }

      ArrayList<Parameter> parameters = new ArrayList<>();
      int startingPos = opString.length() - 3; // First value that is not part of the operation
      for (int i = startingPos, j = 1; i >= 0; i--, j++) {
        boolean isTarget = i == 0 && op.writeToMemory;
        parameters.add(
            Parameter.from(
                ParameterMode.from(opString.charAt(i)), rawInstruction.get(j), isTarget));
      }

      return new Instruction(operation, parameters);
    }

    private boolean isValid() {
      return this.operation.getParamSize() == this.parameters.size();
    }
  }

  @AllArgsConstructor
  @Getter
  enum Operation {
    SUM(1, 3, true),
    MULTIPLY(2, 3, true),
    READ(3, 1, true),
    WRITE(4, 1, true),
    JUMP_IF_TRUE(5, 2, false),
    JUMP_IF_FALSE(6, 2, false),
    LESS_THAN(7, 3, true),
    EQUALS(8, 3, true),
    ADJUST_RELATIVE_BASE(9, 1, false),
    EXIT(99, 0, false);

    private final int repr;
    private final int paramSize;
    private final boolean writeToMemory;

    public static Operation from(char charRepr) {
      return from(Character.getNumericValue(charRepr));
    }

    public static Operation from(final long intRepr) {
      long parsedInput = intRepr;
      String opString = String.valueOf(intRepr);
      if (opString.length() > 1 && !opString.equals("99")) {
        parsedInput = Character.getNumericValue(opString.charAt(opString.length() - 1));
      }

      return switch (Long.valueOf(parsedInput).intValue()) {
        case 1 -> SUM;
        case 2 -> MULTIPLY;
        case 3 -> READ;
        case 4 -> WRITE;
        case 5 -> JUMP_IF_TRUE;
        case 6 -> JUMP_IF_FALSE;
        case 7 -> LESS_THAN;
        case 8 -> EQUALS;
        case 9 -> ADJUST_RELATIVE_BASE;
        case 99 -> EXIT;
        default -> {
          throw new RuntimeException("Received invalid Operation: " + parsedInput);
        }
      };
    }
  }

  @AllArgsConstructor(access = AccessLevel.PRIVATE)
  @Getter
  static class Parameter {

    private final ParameterMode mode;
    private final long value;
    private final boolean isTarget;

    public static Parameter from(
        final ParameterMode mode, final long value, final boolean isTarget) {
      return new Parameter(mode, value, isTarget);
    }
  }

  @AllArgsConstructor(access = AccessLevel.PRIVATE)
  @Getter
  enum ParameterMode {
    UNKNOWN(Integer.MIN_VALUE),
    POSITION(0),
    IMMEDIATE(1),
    RELATIVE(2);

    private final int repr;

    public static ParameterMode from(char charRepr) {
      return from(Character.getNumericValue(charRepr));
    }

    public static ParameterMode from(int intRepr) {
      return switch (intRepr) {
        case 0 -> POSITION;
        case 1 -> IMMEDIATE;
        case 2 -> RELATIVE;
        default -> {
          throw new RuntimeException("Received invalid ParameterMode: " + intRepr);
        }
      };
    }
  }
}
