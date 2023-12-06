package se.augustocesar.aoc2019.shared.intcomputer.exceptions;

public class InvalidOpCode extends RuntimeException {

  public InvalidOpCode(String message) {
    super(message);
  }

  public InvalidOpCode(int code) {
    super(String.format("Unexpected OpCode: %d", code));
  }
}
