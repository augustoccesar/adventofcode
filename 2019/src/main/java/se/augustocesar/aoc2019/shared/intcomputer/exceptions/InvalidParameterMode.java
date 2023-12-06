package se.augustocesar.aoc2019.shared.intcomputer.exceptions;

public class InvalidParameterMode extends RuntimeException {

  public InvalidParameterMode(int code) {
    super(String.format("Unexpected ParameterMode: %d", code));
  }
}
