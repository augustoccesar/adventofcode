package se.augustocesar.aocjava.utils;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class Pair<L, R> {
  private final L left;
  private final R right;

  public static <L, R> Pair<L, R> of(L left, R right) {
    return new Pair<>(left, right);
  }
}
