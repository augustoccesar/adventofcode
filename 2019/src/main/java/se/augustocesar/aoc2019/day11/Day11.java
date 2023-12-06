package se.augustocesar.aoc2019.day11;

import java.io.IOException;
import se.augustocesar.aoc2019.task.RunnableTask;
import se.augustocesar.aoc2019.task.Task;

@RunnableTask(day = 11)
public class Day11 extends Task {

  @Override
  public String partOne() {
    String program = this.readInput();
    Robot robot = Robot.load(program);

    robot.start();

    return String.valueOf(robot.getPositionTracker().size());
  }

  @Override
  public String partTwo() {
    String program = this.readInput();
    Robot robot = Robot.load(program);

    robot.start(PanelColor.WHITE);

    robot.printPositionHistory();

    return "JKZLZJBH"; // Printed by the command above
  }
}
