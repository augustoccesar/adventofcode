package com.augustoccesar.adventofcode.shared.intcomputer.exceptions;

import com.augustoccesar.adventofcode.shared.intcomputer.Parameter.Mode;

public class UnsupportedParameterMode extends RuntimeException {

  public UnsupportedParameterMode(final Mode parameterMode) {
    super(String.format("Received unsupported ParameterMode %s", parameterMode.name()));
  }
}
