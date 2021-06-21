package com.augustoccesar.adventofcode.day13;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
enum TileType {
  EMPTY(0, '.'),
  WALL(1, '@'),
  BLOCK(2, '#'),
  HORIZONTAL_PADDLE(3, '-'),
  BALL(4, 'o');

  @Getter
  private final int id;

  @Getter
  private final char repr;

  public static TileType valueOf(final int id) {
    return switch (id) {
      case 0 -> EMPTY;
      case 1 -> WALL;
      case 2 -> BLOCK;
      case 3 -> HORIZONTAL_PADDLE;
      case 4 -> BALL;
      default -> throw new IllegalStateException("Unexpected value: " + id);
    };
  }
}
