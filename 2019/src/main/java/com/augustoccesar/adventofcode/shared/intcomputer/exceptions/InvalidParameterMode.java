package com.augustoccesar.adventofcode.shared.intcomputer.exceptions;

public class InvalidParameterMode extends RuntimeException {

  public InvalidParameterMode(int code) {
    super(String.format("Unexpected ParameterMode: %d", code));
  }
}
