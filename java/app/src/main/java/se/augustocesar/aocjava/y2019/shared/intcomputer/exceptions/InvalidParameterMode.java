package se.augustocesar.aocjava.y2019.shared.intcomputer.exceptions;

public class InvalidParameterMode extends RuntimeException {

  public InvalidParameterMode(int code) {
    super(String.format("Unexpected ParameterMode: %d", code));
  }
}
