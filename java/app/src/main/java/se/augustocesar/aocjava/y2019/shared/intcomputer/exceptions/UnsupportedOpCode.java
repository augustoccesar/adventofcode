package se.augustocesar.aocjava.y2019.shared.intcomputer.exceptions;

import se.augustocesar.aocjava.y2019.shared.intcomputer.OpCode;

public class UnsupportedOpCode extends RuntimeException {

  public UnsupportedOpCode(final OpCode opCode) {
    super(String.format("IntComputer does not support OpCode %s", opCode.name()));
  }
}
