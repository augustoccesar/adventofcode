package com.augustoccesar.adventofcode.day13;

import com.augustoccesar.adventofcode.shared.intcomputer.IntComputer;
import com.augustoccesar.adventofcode.utils.Console;
import com.augustoccesar.adventofcode.utils.MinMax;
import com.augustoccesar.adventofcode.utils.Pair;
import com.augustoccesar.adventofcode.utils.Point2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.SneakyThrows;

public class Arcade {

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

    Pair<MinMax<Integer>, MinMax<Integer>> minMaxes = Point2D.minMaxes(
        this.tiles.stream()
            .map(Tile::getPosition)
            .collect(Collectors.toList())
    );

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
