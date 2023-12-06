package se.augustocesar.aoc2019.shared.intcomputer;

import java.util.List;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Instruction {

  @Getter
  private OpCode opCode;

  @Getter
  private List<Parameter> parameters;

  public static Instruction of(final OpCode opCode, final List<Parameter> parameters) {
    return new Instruction(opCode, parameters);
  }

  public boolean isParamWriteToMemory(final int parameterIdx) {
    return this.opCode.isWriter() && parameterIdx == opCode.getParamSize() - 1;
  }
}
