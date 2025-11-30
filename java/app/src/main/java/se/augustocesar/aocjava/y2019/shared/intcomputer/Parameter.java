package se.augustocesar.aocjava.y2019.shared.intcomputer;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import se.augustocesar.aocjava.y2019.shared.intcomputer.exceptions.InvalidParameterMode;

@AllArgsConstructor(access = AccessLevel.PUBLIC)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Parameter {

  @Getter private Mode mode;

  @Getter private long value;

  public static Parameter of(final Mode mode, final long value) {
    return new Parameter(mode, value);
  }

  @AllArgsConstructor
  public enum Mode {
    POSITION(0),
    IMMEDIATE(1),
    RELATIVE(2);

    @Getter private final int code;

    public static Mode from(final int code) {
      return switch (code) {
        case 0 -> POSITION;
        case 1 -> IMMEDIATE;
        case 2 -> RELATIVE;
        default -> throw new InvalidParameterMode((code));
      };
    }
  }
}
