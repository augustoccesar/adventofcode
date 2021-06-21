package com.augustoccesar.adventofcode.day11;

import com.augustoccesar.adventofcode.shared.intcomputer.IntComputer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.Getter;

class Robot {

  private static final Integer[][] moveModifier = new Integer[][]{
      new Integer[]{0, 1}, // up
      new Integer[]{1, 0}, // right
      new Integer[]{0, -1}, // down
      new Integer[]{-1, 0}, // left
  };

  private int facing = 0; // up = 0; right = 1; down = 2; left = 3
  private final int[] position = new int[]{0, 0};

  @Getter
  private final Map<String, PanelColor> positionTracker = new HashMap<>();

  private final IntComputer internalComputer;

  private Robot(final String program) {
    this.internalComputer = IntComputer.load(program);
  }

  public static Robot load(final String program) {
    return new Robot(program);
  }

  public void start() {
    start(null);
  }

  public void start(final PanelColor firstCameraInput) {
    boolean firstLoop = true;

    while (!this.internalComputer.isHalted()) {
      if (firstLoop && firstCameraInput != null) {
        this.internalComputer.getInputSource().write(firstCameraInput.getIntRepr());
        firstLoop = false;
      } else {
        this.internalComputer.getInputSource().write(this.cameraInput().getIntRepr());
      }

      this.internalComputer.runUntilHaltedOrPaused();
      int colorToPaint = (int) this.internalComputer.outputRead();

      this.internalComputer.runUntilHaltedOrPaused();
      int turnDirection = (int) this.internalComputer.outputRead();

      this.paint(PanelColor.from(colorToPaint));
      this.turn(Turn.from(turnDirection));
    }
  }

  private PanelColor cameraInput() {
    final String positionKey = coordsToString(position);
    if (!this.positionTracker.containsKey(positionKey)) {
      this.positionTracker.put(positionKey, PanelColor.BLACK);
    }

    return this.positionTracker.get(positionKey);
  }

  private void turn(final Turn turn) {
    int newDirection = this.facing;

    // Since the turns are always made by increment or decrement of 1 and the amount of
    // directions is static, no need to worry about overflows with mod(%), etc
    if (turn == Turn.LEFT) {
      newDirection--;
      if (newDirection < 0) {
        newDirection = 3;
      }
    } else if (turn == Turn.RIGHT) {
      newDirection++;
      if (newDirection > 3) {
        newDirection = 0;
      }
    }

    Integer[] modifier = moveModifier[newDirection];

    this.facing = newDirection;
    this.position[0] += modifier[0];
    this.position[1] += modifier[1];
  }

  private void paint(final PanelColor color) {
    final String positionKey = coordsToString(position);
    this.positionTracker.put(positionKey, color);
  }

  private String coordsToString(final int[] coords) {
    if (coords.length != 2) {
      throw new IllegalArgumentException(
          String.format("Invalid size for coords array. Expected: 2; Received: %d", coords.length)
      );
    }

    return String.format("(%d,%d)", coords[0], coords[1]);
  }

  public void printPositionHistory() {
    List<Integer[]> positions = this.positionTracker.keySet().stream().map(it -> {
      String[] tokens = it
          .replace("(", "")
          .replace(")", "")
          .split(",");

      int x = Integer.parseInt(tokens[0]);
      int y = Integer.parseInt(tokens[1]);

      return new Integer[]{x, y};
    }).collect(Collectors.toList());

    int minX = Integer.MAX_VALUE;
    int maxX = Integer.MIN_VALUE;
    int minY = Integer.MAX_VALUE;
    int maxY = Integer.MIN_VALUE;

    for (final Integer[] pos : positions) {
      if (pos[0] > maxX) {
        maxX = pos[0];
      }
      if (pos[0] < minX) {
        minX = pos[0];
      }

      if (pos[1] > maxY) {
        maxY = pos[1];
      }
      if (pos[1] < minY) {
        minY = pos[1];
      }
    }

    for (int row = maxY; row >= minY; row--) {
      for (int col = minX; col <= maxX; col++) {
        String key = this.coordsToString(new int[]{col, row});
        PanelColor panel = this.positionTracker.get(key);
        if (panel == null) {
          panel = PanelColor.BLACK;
        }

        System.out.printf("%c ", panel.getCharRepr());
      }
      System.out.println();
    }
  }
}
