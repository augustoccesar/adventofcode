package com.augustoccesar.aocjava.y2019.shared.intcomputer;

import java.util.HashMap;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Memory {

  @Getter private HashMap<Long, Long> underlying = new HashMap<>();

  public static Memory initialize(final String program) {
    final HashMap<Long, Long> memory = new HashMap<>();
    final String[] tokens = program.split(",");

    for (int i = 0; i < tokens.length; i++) {
      memory.put((long) i, Long.parseLong(tokens[i]));
    }

    return new Memory(memory);
  }

  public boolean contains(final long position) {
    return underlying.containsKey(position);
  }

  public long read(final long position) {
    return this.underlying.get(position);
  }

  public long readOrDefault(final long position, final long defaultValue) {
    if (!this.contains(position)) {
      this.write(position, defaultValue);
    }

    return this.read(position);
  }

  public void write(final long pos, final long val) {
    this.underlying.put(pos, val);
  }

  public void reset() {
    this.underlying.clear();
  }

  public int size() {
    return this.underlying.size();
  }
}
