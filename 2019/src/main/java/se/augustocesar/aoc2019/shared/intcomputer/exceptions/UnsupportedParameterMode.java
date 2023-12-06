package se.augustocesar.aoc2019.shared.intcomputer.exceptions;

import se.augustocesar.aoc2019.shared.intcomputer.Parameter.Mode;

public class UnsupportedParameterMode extends RuntimeException {

  public UnsupportedParameterMode(final Mode parameterMode) {
    super(String.format("Received unsupported ParameterMode %s", parameterMode.name()));
  }
}
