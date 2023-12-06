package se.augustocesar.aoc2019.day13;

import se.augustocesar.aoc2019.Task;
import java.io.IOException;

public class Day13 extends Task {

  @Override
  public String partOne() throws IOException {
    final String game = this.readInput();
    final Arcade arcade = Arcade.load(game);

    arcade.run(false);

    final long result = arcade.getTiles().stream()
        .filter(it -> it.getType() == TileType.BLOCK)
        .count();

    return String.valueOf(result);
  }

  @Override
  public String partTwo() throws IOException {
    final String game = this.readInput();
    final Arcade arcade = Arcade.load(game);
    arcade.modifyMemory(0, 2); // Free to play

    arcade.run(false);

    return String.valueOf(arcade.getScore());
  }
}
