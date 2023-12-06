package se.augustocesar.aoc2019.day13;

import se.augustocesar.aoc2019.task.RunnableTask;
import se.augustocesar.aoc2019.task.Task;

@RunnableTask(day = 13)
public class Day13 extends Task {

  @Override
  public String partOne() {
    final String game = this.readInput();
    final Arcade arcade = Arcade.load(game);

    arcade.run(false);

    final long result = arcade.getTiles().stream()
        .filter(it -> it.getType() == TileType.BLOCK)
        .count();

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
