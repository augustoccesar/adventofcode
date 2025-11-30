package se.augustocesar.aocjava.y2019.shared.intcomputer;

import java.util.LinkedList;
import lombok.Getter;

public class MemoryInputSource extends InputSource {

  private final MemoryInputAccessMode accessMode;

  @Getter private final LinkedList<Long> memory = new LinkedList<>();

  private MemoryInputSource() {
    this(MemoryInputAccessMode.PEEK_LAST);
  }

  private MemoryInputSource(final MemoryInputAccessMode accessMode) {
    this.accessMode = accessMode;
  }

  public static MemoryInputSource with(final MemoryInputAccessMode inputAccessMode) {
    return new MemoryInputSource(inputAccessMode);
  }

  @Override
  public long read() {
    return switch (this.accessMode) {
      case PEEK_LAST -> this.memory.peekLast();
      case POOL_FIRST -> this.memory.pollFirst();
    };
  }

  @Override
  public void write(final long value) {
    this.memory.add(value);
  }

  @Override
  public void clear() {
    this.memory.clear();
  }
}
