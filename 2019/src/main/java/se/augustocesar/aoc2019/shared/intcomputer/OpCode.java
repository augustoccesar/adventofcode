package se.augustocesar.aoc2019.shared.intcomputer;

import se.augustocesar.aoc2019.shared.intcomputer.exceptions.InvalidOpCode;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
public enum OpCode {
  ADD(1, 3, 4, true),
  MULTIPLY(2, 3, 4, true),
  READ_INPUT(3, 1, 2, true),
  WRITE_OUTPUT(4, 1, 2, false),
  JUMP_IF_TRUE(5, 2, 3, false),
  JUMP_IF_FALSE(6, 2, 3, false),
  LESS_THAN(7, 3, 4, true),
  EQUALS(8, 3, 4, true),
  ADJUST_RELATIVE_BASE(9, 1, 2, false),
  HALT(99, 0, 1, false);

  @Getter
  private final int intCode;

  @Getter
  private final int paramSize;

  @Getter
  private final int pointerJump;

  @Getter()
  private final boolean writer;

  public static OpCode from(final Long code) {
    return switch (code.intValue()) {
      case 1 -> ADD;
      case 2 -> MULTIPLY;
      case 3 -> READ_INPUT;
      case 4 -> WRITE_OUTPUT;
      case 5 -> JUMP_IF_TRUE;
      case 6 -> JUMP_IF_FALSE;
      case 7 -> LESS_THAN;
      case 8 -> EQUALS;
      case 9 -> ADJUST_RELATIVE_BASE;
      case 99 -> HALT;
      default -> throw new InvalidOpCode(code.intValue());
    };
  }
}
