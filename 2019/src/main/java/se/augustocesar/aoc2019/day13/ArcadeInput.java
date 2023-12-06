package se.augustocesar.aoc2019.day13;

import se.augustocesar.aoc2019.shared.intcomputer.InputSource;

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
