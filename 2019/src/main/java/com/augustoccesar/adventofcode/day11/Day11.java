package com.augustoccesar.adventofcode.day11;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;

public class Day11 extends Task {

  @Override
  public String partOne() throws IOException {
    String program = this.readInput();
    Robot robot = Robot.load(program);

    robot.start();

    return String.valueOf(robot.getPositionTracker().size());
  }

  @Override
  public String partTwo() throws IOException {
    String program = this.readInput();
    Robot robot = Robot.load(program);

    robot.start(PanelColor.WHITE);

    robot.printPositionHistory();

    return "JKZLZJBH"; // Printed by the command above
  }
}
