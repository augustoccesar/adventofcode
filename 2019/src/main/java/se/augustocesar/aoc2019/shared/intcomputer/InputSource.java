package se.augustocesar.aoc2019.shared.intcomputer;

public abstract class InputSource {

  public abstract long read();

  public void write(final long value) {
    throw new UnsupportedOperationException();
  }

  public void clear() {
    throw new UnsupportedOperationException();
  }
}
