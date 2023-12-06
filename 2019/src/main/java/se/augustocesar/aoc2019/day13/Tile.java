package se.augustocesar.aoc2019.day13;

import se.augustocesar.aoc2019.utils.Point2D;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
class Tile {

  private Point2D position;
  private TileType type;

  public static Tile of(final int x, final int y, final int tileTypeId) {
    return new Tile(Point2D.on(x, y), TileType.valueOf(tileTypeId));
  }

  public static Tile of(final Point2D position, final int tileTypeId) {
    return new Tile(position, TileType.valueOf(tileTypeId));
  }
}
