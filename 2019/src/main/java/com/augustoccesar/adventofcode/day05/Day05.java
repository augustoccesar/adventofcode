package com.augustoccesar.adventofcode.day05;

import com.augustoccesar.adventofcode.BaseDay;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.ToString;

import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Parameter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day05 extends BaseDay {
    @Override
    public void partOne() throws IOException {
        String program = this.readInput().strip();
        ArrayList<Integer> memory = Stream.of(program.split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toCollection(ArrayList::new));
        int input = 1;
        int lastOutput = Integer.MIN_VALUE;

        for (int i = 0; ; ) {
            Operation op = Operation.from(memory.get(i));

            if (op == Operation.EXIT) {
                break;
            }

            Instruction instruction = Instruction.from(memory.subList(i, i + op.getParamSize() + 1));
            Optional<Integer> output = instruction.apply(input, memory);
            if(output.isPresent()) {
                lastOutput = output.get();
            }

            i += op.getParamSize() + 1;
        }

        System.out.println("Part One: " + lastOutput);
    }

    @Override
    public void partTwo() throws IOException {
        System.out.println("Part Two: Not implemented");
    }

    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    @Getter
    static class Instruction {
        private final Operation operation;
        private final List<Parameter> parameters;

        public static Instruction from(final List<Integer> rawInstruction) {
            Operation op = Operation.from(rawInstruction.get(0));

            if (op == Operation.UNKNOWN) {
                throw new RuntimeException("Found unknown operation");
            }

            if (op == Operation.EXIT) {
                return new Instruction(
                        Operation.EXIT,
                        Collections.emptyList()
                );
            }

            if (op == Operation.READ || op == Operation.WRITE) {
                return new Instruction(
                        Operation.from(rawInstruction.get(0)),
                        List.of(Parameter.from(ParameterMode.IO, rawInstruction.get(1)))
                );
            }

            if (rawInstruction.get(0) == 99) {
                return new Instruction(
                        Operation.EXIT,
                        Collections.emptyList()
                );
            }

            String opString = String.valueOf(rawInstruction.get(0));
            if (opString.length() < 5) {
                int diff = 5 - opString.length();
                opString = "0".repeat(diff) + opString;
            }

            final Operation operation = Operation.from(opString.charAt(4)); // Op code
            final List<Parameter> parameters = List.of(
                    Parameter.from(ParameterMode.from(opString.charAt(2)), rawInstruction.get(1)),
                    Parameter.from(ParameterMode.from(opString.charAt(1)), rawInstruction.get(2)),
                    Parameter.from(ParameterMode.IO, rawInstruction.get(3))
            );

            return new Instruction(
                    operation,
                    parameters
            );
        }

        public Optional<Integer> apply(int input, ArrayList<Integer> memory) {
            if (!this.isValid()) {
                throw new RuntimeException(String.format("Applying invalid instruction: %s", this));
            }

            if (this.operation == Operation.EXIT) {
                return Optional.empty();
            }

            if (this.operation == Operation.READ) {
                // Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
                // For example, the instruction 3,50 would take an input value and store it at address 50.
                memory.set(this.parameters.get(0).getValue(), input);
                return Optional.empty();
            }

            if (this.operation == Operation.WRITE) {
                // Opcode 4 outputs the value of its only parameter. For example, the instruction 4,50
                // would output the value at address 50.
                return Optional.of(memory.get(this.parameters.get(0).getValue()));
            }

            if (this.operation == Operation.SUM || this.operation == Operation.MULTIPLY) {
                // TODO: There is probably a less verbose way of doing this parsing of the params
                int param1Value, param2Value;
                int targetIndex = this.getParameters().get(2).getValue();
                Parameter param1 = this.parameters.get(0);
                Parameter param2 = this.parameters.get(1);

                if (param1.getMode() == ParameterMode.POSITION) {
                    param1Value = memory.get(param1.value);
                } else if (param1.getMode() == ParameterMode.IMMEDIATE) {
                    param1Value = param1.value;
                } else {
                    throw new RuntimeException(String.format("Invalid mode for parameter 1: %s", param1.getMode()));
                }

                if (param2.getMode() == ParameterMode.POSITION) {
                    param2Value = memory.get(param2.value);
                } else if (param2.getMode() == ParameterMode.IMMEDIATE) {
                    param2Value = param2.value;
                } else {
                    throw new RuntimeException(String.format("Invalid mode for parameter 2: %s", param2.getMode()));
                }

                switch (this.operation) {
                    case SUM -> memory.set(targetIndex, param1Value + param2Value);
                    case MULTIPLY -> memory.set(targetIndex, param1Value * param2Value);
                }
            }

            return Optional.empty();
        }

        private boolean isValid() {
            return this.operation.getParamSize() == this.parameters.size();
        }
    }

    @AllArgsConstructor
    @Getter
    enum Operation {
        UNKNOWN(Integer.MIN_VALUE, 0),
        SUM(1, 3),
        MULTIPLY(2, 3),
        READ(3, 1),
        WRITE(4, 1),
        EXIT(99, 0);

        private final int repr;
        private final int paramSize;

        public static Operation from(char charRepr) {
            return from(Character.getNumericValue(charRepr));
        }

        public static Operation from(final int intRepr) {
            int parsedInput = intRepr;
            String opString = String.valueOf(intRepr);
            if (opString.length() > 1 && !opString.equals("99")) {
                parsedInput = Character.getNumericValue(opString.charAt(opString.length() - 1));
            }

            return switch (parsedInput) {
                case 1 -> SUM;
                case 2 -> MULTIPLY;
                case 3 -> READ;
                case 4 -> WRITE;
                case 99 -> EXIT;
                default -> UNKNOWN;
            };
        }
    }

    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    @Getter
    static class Parameter {
        private final ParameterMode mode;
        private final int value;

        public static Parameter from(final ParameterMode mode, final char charValue) {
            return new Parameter(mode, Character.getNumericValue(charValue));
        }

        public static Parameter from(final ParameterMode mode, final int value) {
            return new Parameter(mode, value);
        }
    }

    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    @Getter
    enum ParameterMode {
        UNKNOWN(Integer.MIN_VALUE),
        IO(-1),
        POSITION(0),
        IMMEDIATE(1);

        private final int repr;

        public static ParameterMode from(char charRepr) {
            return from(Character.getNumericValue(charRepr));
        }

        public static ParameterMode from(int intRepr) {
            return switch (intRepr) {
                case 0 -> POSITION;
                case 1 -> IMMEDIATE;
                default -> UNKNOWN;
            };
        }
    }
}
