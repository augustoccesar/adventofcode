package com.augustoccesar.adventofcode.day05;

import com.augustoccesar.adventofcode.BaseDay;
import com.augustoccesar.adventofcode.utils.Pair;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Day05 extends BaseDay {
    @Override
    public void partOne() throws IOException {
        String program = this.readInput().strip();
        int result = runProgram(program, 1);

        System.out.println("Part One: " + result);
    }

    @Override
    public void partTwo() throws IOException {
        String program = this.readInput().strip();
        int result = runProgram(program, 5);

        System.out.println("Part Two: " + result);
    }

    private int runProgram(final String program, final int input) {
        ArrayList<Integer> memory = Stream.of(program.split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toCollection(ArrayList::new));
        int lastOutput = Integer.MIN_VALUE;

        for (int i = 0; ; ) {
            Operation op = Operation.from(memory.get(i));

            if (op == Operation.EXIT) {
                break;
            }

            Instruction instruction = Instruction.from(memory.subList(i, i + op.getParamSize() + 1));

            Optional<Pair<Instruction.ApplyResult, Integer>> output = instruction.apply(input, memory);
            if (output.isPresent()) {
                final Pair<Instruction.ApplyResult, Integer> pair = output.get();
                switch (output.get().getLeft()) {
                    case Output -> lastOutput = pair.getRight();
                    case ModifyInstructorPointer -> {
                        i = pair.getRight();
                        continue;
                    }
                }
            }

            i += op.getParamSize() + 1;
        }

        return lastOutput;
    }

    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    @Getter
    static class Instruction {
        enum ApplyResult {
            Output, ModifyInstructorPointer
        }

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
            final Operation operation = Operation.from(opString.charAt(opString.length() - 1));
            final int maxOpStringSize = 2 + operation.paramSize;
            if (opString.length() < maxOpStringSize) {
                int diff = maxOpStringSize - opString.length();
                opString = "0".repeat(diff) + opString;
            }

            ArrayList<Parameter> parameters = new ArrayList<>();
            int startingPos = opString.length() - 3; // First value that is not part of the operation
            for (int i = startingPos, j = 1; i >= 0; i--, j++) {
                if (i == 0 && op.writeToMemory) { // If it writes, the last loop is a IO parameter
                    parameters.add(
                            Parameter.from(ParameterMode.IO, rawInstruction.get(j))
                    );
                    continue;
                }

                parameters.add(
                        Parameter.from(ParameterMode.from(opString.charAt(i)), rawInstruction.get(j))
                );
            }

            return new Instruction(
                    operation,
                    parameters
            );
        }

        public Optional<Pair<ApplyResult, Integer>> apply(int input, ArrayList<Integer> memory) {
            if (!this.isValid()) {
                throw new RuntimeException(String.format("Applying invalid instruction: %s", this));
            }

            if (this.operation == Operation.EXIT) {
                return Optional.empty();
            }

            if (this.operation == Operation.READ) {
                memory.set(this.parameters.get(0).getValue(), input);
                return Optional.empty();
            }

            if (this.operation == Operation.WRITE) {
                return Optional.of(
                        Pair.of(
                                ApplyResult.Output,
                                memory.get(this.parameters.get(0).getValue())
                        )
                );
            }

            List<Integer> paramValues = this.getParameters().stream()
                    .map(item -> {
                        if (item.getMode() == ParameterMode.POSITION) {
                            return memory.get(item.getValue());
                        } else if (item.getMode() == ParameterMode.IMMEDIATE || item.getMode() == ParameterMode.IO) {
                            return item.getValue();
                        } else {
                            throw new RuntimeException(String.format("Invalid mode for parameter: %s", item.getMode()));
                        }
                    }).collect(Collectors.toList());
            int lastIndex = paramValues.size() - 1;

            switch (this.operation) {
                case SUM -> memory.set(paramValues.get(lastIndex), paramValues.stream().limit(lastIndex).reduce(0, Integer::sum));
                case MULTIPLY -> memory.set(paramValues.get(lastIndex), paramValues.stream().limit(lastIndex).reduce(1, Math::multiplyExact));
                case JUMP_IF_TRUE -> {
                    if (paramValues.get(0) != 0) {
                        return Optional.of(Pair.of(ApplyResult.ModifyInstructorPointer, paramValues.get(1)));
                    }
                }
                case JUMP_IF_FALSE -> {
                    if (paramValues.get(0) == 0) {
                        return Optional.of(Pair.of(ApplyResult.ModifyInstructorPointer, paramValues.get(1)));
                    }
                }
                case LESS_THAN -> {
                    if (paramValues.get(0) < paramValues.get(1)) {
                        memory.set(paramValues.get(2), 1);
                    } else {
                        memory.set(paramValues.get(2), 0);
                    }
                }
                case EQUALS -> {
                    if (paramValues.get(0).equals(paramValues.get(1))) {
                        memory.set(paramValues.get(2), 1);
                    } else {
                        memory.set(paramValues.get(2), 0);
                    }
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
        UNKNOWN(Integer.MIN_VALUE, 0, false),
        SUM(1, 3, true),
        MULTIPLY(2, 3, true),
        READ(3, 1, true),
        WRITE(4, 1, true),
        JUMP_IF_TRUE(5, 2, false),
        JUMP_IF_FALSE(6, 2, false),
        LESS_THAN(7, 3, true),
        EQUALS(8, 3, true),
        EXIT(99, 0, false);

        private final int repr;
        private final int paramSize;
        private final boolean writeToMemory;

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
                case 5 -> JUMP_IF_TRUE;
                case 6 -> JUMP_IF_FALSE;
                case 7 -> LESS_THAN;
                case 8 -> EQUALS;
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
