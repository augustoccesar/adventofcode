package se.augustocesar.aocjava.y2019;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.SneakyThrows;
import se.augustocesar.aocjava.Day;
import se.augustocesar.aocjava.RunnableDay;
import se.augustocesar.aocjava.utils.Console;
import se.augustocesar.aocjava.utils.MinMax;
import se.augustocesar.aocjava.utils.Pair;
import se.augustocesar.aocjava.utils.Point2D;
import se.augustocesar.aocjava.y2019.shared.intcomputer.InputSource;
import se.augustocesar.aocjava.y2019.shared.intcomputer.IntComputer;

@RunnableDay(year = 2019, day = 13)
public class Day13 extends Day {

  @Override
  public String partOne() {
    final String game = this.readInput();
    final Arcade arcade = Arcade.load(game);

    arcade.run(false);

    final long result =
        arcade.getTiles().stream().filter(it -> it.getType() == TileType.BLOCK).count();

    return String.valueOf(result);
  }

  @Override
  public String partTwo() {
    final String game = this.readInput();
    final Arcade arcade = Arcade.load(game);
    arcade.modifyMemory(0, 2); // Free to play

    arcade.run(false);

    return String.valueOf(arcade.getScore());
  }
}

@AllArgsConstructor
enum TileType {
  EMPTY(0, '.'),
  WALL(1, '@'),
  BLOCK(2, '#'),
  HORIZONTAL_PADDLE(3, '-'),
  BALL(4, 'o');

  @Getter private final int id;

  @Getter private final char repr;

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

class Arcade {
  private final IntComputer internalComputer;

  @Getter private final List<Tile> tiles = new ArrayList<>();
  private final HashMap<String, Tile> tilesFastAccess = new HashMap<>();

  private boolean visualize = false;
  private boolean loadedAllTiles = false;

  @Getter private long score = 0;
  @Getter private Point2D ballPos;
  @Getter private Point2D paddlePos;

  private Arcade(final String game) {
    this.internalComputer = IntComputer.load(game, new ArcadeInput(this));
  }

  public static Arcade load(final String game) {
    return new Arcade(game);
  }

  public void modifyMemory(final long position, final long value) {
    this.internalComputer.getMemory().write(position, value);
  }

  public void run(final boolean visualize) {
    this.visualize = visualize;

    while (!this.internalComputer.isHalted()) {
      this.internalComputer.runUntilHaltedOrPaused();
      if (this.internalComputer.getOutput().size() % 3 == 0) {
        List<Long> output = this.internalComputer.outputRead(3);
        Point2D tilePosition = Point2D.on(output.get(0).intValue(), output.get(1).intValue());

        if (tilePosition.getX() == -1 && tilePosition.getY() == 0) {
          this.score = output.get(2);
          this.loadedAllTiles = true;
        } else {
          final Tile tile = Tile.of(tilePosition, output.get(2).intValue());
          this.tiles.add(tile);
          this.tilesFastAccess.put(tile.getPosition().id(), tile);

          if (tile.getType() == TileType.BALL) {
            this.ballPos = tile.getPosition();
          } else if (tile.getType() == TileType.HORIZONTAL_PADDLE) {
            this.paddlePos = tile.getPosition();
          }
        }

        this.printDisplay();
      }
    }
  }

  @SneakyThrows
  public void printDisplay() {
    if (!this.loadedAllTiles || !this.visualize) {
      return;
    }

    Pair<MinMax<Integer>, MinMax<Integer>> minMaxes =
        Point2D.minMaxes(this.tiles.stream().map(Tile::getPosition).collect(Collectors.toList()));

    MinMax<Integer> minMaxX = minMaxes.getLeft();
    MinMax<Integer> minMaxY = minMaxes.getRight();

    Console.clearScreen();
    for (int row = minMaxY.getMin(); row <= minMaxY.getMax(); row++) {
      for (int col = minMaxX.getMin(); col <= minMaxX.getMax(); col++) {
        String key = Point2D.on(col, row).id();
        Tile tile = this.tilesFastAccess.get(key);

        System.out.printf("%c", tile.getType().getRepr());
      }
      System.out.println();
    }

    Thread.sleep(100);
  }
}

class ArcadeInput extends InputSource {
  private final Arcade arcade;

  public ArcadeInput(Arcade arcade) {
    this.arcade = arcade;
  }

  @Override
  public long read() {
    return Integer.compare(this.arcade.getBallPos().getX(), this.arcade.getPaddlePos().getX());
  }
}
