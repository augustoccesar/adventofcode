package com.augustoccesar.adventofcode.day13;

import com.augustoccesar.adventofcode.shared.intcomputer.InputSource;

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
