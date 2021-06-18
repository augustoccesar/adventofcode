package com.augustoccesar.adventofcode.day13;

import com.augustoccesar.adventofcode.Task;
import com.augustoccesar.adventofcode.shared.intcomputer.IntComputer;
import java.io.IOException;

public class Day13 extends Task {

  @Override
  public String partOne() throws IOException {
    final String game = this.readInput();
    final IntComputer arcade = IntComputer.load(game);
    int visibleBlocks = 0;

    while (!arcade.isHalted()) {
      arcade.runUntilHaltedOrPaused();
      if (arcade.getOutput().size() % 3 == 0) {
        int tileType = (int) arcade.outputRead();
        if (TileType.valueOf(tileType) == TileType.BLOCK) {
          visibleBlocks++;
        }
      }
    }

    return String.valueOf(visibleBlocks);
  }

  @Override
  public String partTwo() {
    return "-";
  }
}
