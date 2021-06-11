package com.augustoccesar.adventofcode.shared.intcomputer;

import com.augustoccesar.adventofcode.shared.intcomputer.exceptions.UnsupportedOpCode;
import com.augustoccesar.adventofcode.shared.intcomputer.exceptions.UnsupportedParameterMode;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class IntComputer {

  private @NonNull String program;

  @Getter
  private @NonNull Memory memory;

  @Getter
  private long instructionPointer = 0;

  @Getter
  private long relativeBase = 0;

  @Getter
  private boolean halted = false;

  @Getter
  private boolean paused = false;

  @Getter
  private final LinkedList<Long> input = new LinkedList<>();

  private @NonNull InputAccessMode inputAccessMode;

  @Getter
  private final LinkedList<Long> output = new LinkedList<>();

  @Getter
  private final List<Instruction> ranInstructionsLog = new ArrayList<>();

  public static IntComputer load(final String program) {
    return new IntComputer(program, Memory.initialize(program), InputAccessMode.PEEK_LAST);
  }

  public static IntComputer load(final String program, final InputAccessMode inputAccessMode) {
    return new IntComputer(program, Memory.initialize(program), inputAccessMode);
  }

  public void reset() {
    this.instructionPointer = 0;
    this.halted = false;
    this.memory.reset();
    this.input.clear();
    this.output.clear();
    this.ranInstructionsLog.clear();

    this.memory = Memory.initialize(this.program);
  }

  public void runUntilHalted() {
    while (!this.halted) {
      execute();
    }
  }

  public void runUntilHaltedOrPaused() {
    this.paused = false;
    while (!this.halted && !this.paused) {
      execute();
    }
  }

  private void execute() {
    long currentValue = this.memory.read(this.instructionPointer);
    String currentValueStr = String.valueOf(currentValue);
    if (currentValueStr.length() < 5) {
      final int diff = 5 - currentValueStr.length();
      currentValueStr = "0".repeat(diff) + currentValueStr;
    }

    OpCode opCode = OpCode.from(Long.valueOf(currentValueStr.substring(3, 5)));

    Parameter.Mode[] parameterModes = new Parameter.Mode[]{
        Parameter.Mode.from(Character.getNumericValue(currentValueStr.charAt(2))),
        Parameter.Mode.from(Character.getNumericValue(currentValueStr.charAt(1))),
        Parameter.Mode.from(Character.getNumericValue(currentValueStr.charAt(0)))
    };
    long[] parameterValues = new long[opCode.getParamSize()];
    for (int i = 1; i < parameterValues.length + 1; i++) {
      parameterValues[i - 1] = this.memory.read(this.instructionPointer + i);
    }
    List<Parameter> parameters = new ArrayList<>();
    for (int i = 0; i < parameterValues.length; i++) {
      parameters.add(Parameter.of(parameterModes[i], parameterValues[i]));
    }

    Instruction instruction = Instruction.of(opCode, parameters);
    this.runInstruction(instruction);
  }

  private void runInstruction(final Instruction instruction) {
    boolean jumped = false;

    switch (instruction.getOpCode()) {
      case ADD -> {
        final List<Long> params = this.extractParameters(instruction);

        long result = params.get(0) + params.get(1);
        this.memory.write(params.get(2), result);
      }

      case MULTIPLY -> {
        final List<Long> params = this.extractParameters(instruction);

        long result = params.get(0) * params.get(1);
        this.memory.write(params.get(2), result);
      }

      case READ_INPUT -> {
        final List<Long> params = this.extractParameters(instruction);

        this.memory.write(params.get(0), this.inputRead());
      }

      case WRITE_OUTPUT -> {
        final List<Long> params = this.extractParameters(instruction);

        this.output.add(params.get(0));
        this.paused = true;
      }

      case JUMP_IF_TRUE -> {
        final List<Long> params = this.extractParameters(instruction);

        if (params.get(0) != 0) {
          this.instructionPointer = params.get(1);
          jumped = true;
        }
      }

      case JUMP_IF_FALSE -> {
        final List<Long> params = this.extractParameters(instruction);

        if (params.get(0) == 0) {
          this.instructionPointer = params.get(1);
          jumped = true;
        }
      }

      case LESS_THAN -> {
        final List<Long> params = this.extractParameters(instruction);

        if (params.get(0) < params.get(1)) {
          this.memory.write(params.get(2), 1);
        } else {
          this.memory.write(params.get(2), 0);
        }
      }

      case EQUALS -> {
        final List<Long> params = this.extractParameters(instruction);

        if (params.get(0).equals(params.get(1))) {
          this.memory.write(params.get(2), 1);
        } else {
          this.memory.write(params.get(2), 0);
        }
      }

      case ADJUST_RELATIVE_BASE -> {
        final List<Long> params = this.extractParameters(instruction);

        this.relativeBase += params.get(0);
      }

      case HALT -> this.halt();
      default -> throw new UnsupportedOpCode(instruction.getOpCode());
    }

    if (!jumped) {
      this.stepInstructionPointer(instruction.getOpCode().getPointerJump());
    }

    this.ranInstructionsLog.add(instruction);
  }

  private List<Long> extractParameters(final Instruction instruction) {
    final List<Long> parameters = new ArrayList<>();
    for (int i = 0; i < instruction.getOpCode().getParamSize(); i++) {
      final Parameter parameter = instruction.getParameters().get(i);
      if (instruction.isParamWriteToMemory(i)) {
        switch (parameter.getMode()) {
          case POSITION -> parameters.add(parameter.getValue());
          case IMMEDIATE -> throw new RuntimeException(
              "Write to memory parameter shouldn't be IMMEDIATE"
          );
          case RELATIVE -> parameters.add(parameter.getValue() + this.relativeBase);
          default -> throw new UnsupportedParameterMode(parameter.getMode());
        }
      } else {
        switch (parameter.getMode()) {
          case POSITION -> parameters.add(this.memory.readOrDefault(parameter.getValue(), 0L));
          case IMMEDIATE -> parameters.add(parameter.getValue());
          case RELATIVE -> parameters.add(
              this.memory.readOrDefault(
                  parameter.getValue() + this.relativeBase,
                  0
              )
          );
          default -> throw new UnsupportedParameterMode(parameter.getMode());
        }
      }
    }

    return parameters;
  }

  public void inputWrite(final long value) {
    this.input.add(value);
  }

  private long inputRead() {
    return switch (this.inputAccessMode) {
      case PEEK_LAST -> this.input.peekLast();
      case POOL_FIRST -> this.input.pollFirst();
    };
  }

  public long outputRead() {
    return this.output.peekLast();
  }

  private void stepInstructionPointer(final long steps) {
    this.instructionPointer += steps;
  }

  private void halt() {
    this.halted = true;
  }
}