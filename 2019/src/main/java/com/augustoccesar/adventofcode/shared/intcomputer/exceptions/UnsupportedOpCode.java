package com.augustoccesar.adventofcode.shared.intcomputer.exceptions;

import com.augustoccesar.adventofcode.shared.intcomputer.OpCode;

public class UnsupportedOpCode extends RuntimeException {

  public UnsupportedOpCode(final OpCode opCode) {
    super(String.format("IntComputer does not support OpCode %s", opCode.name()));
  }
}
