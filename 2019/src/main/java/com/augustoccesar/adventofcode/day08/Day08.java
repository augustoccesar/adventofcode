package com.augustoccesar.adventofcode.day08;

import com.augustoccesar.adventofcode.Task;
import java.io.IOException;

public class Day08 extends Task {

  @Override
  public String partOne() throws IOException {
    String data = this.readInput();
    int width = 25;
    int height = 6;

    int layerDataSize = width * height;

    String selectedLayer = null;

    for (int i = 0; i < data.length(); i += layerDataSize) {
      String layer = data.substring(i, Math.min(i + layerDataSize, data.length()));
      if (selectedLayer == null) {
        selectedLayer = layer;
        continue;
      }

      if (countChar(layer, '0') < countChar(selectedLayer, '0')) {
        selectedLayer = layer;
      }
    }

    long result = countChar(selectedLayer, '1') * countChar(selectedLayer, '2');

    return String.valueOf(result);
  }

  @Override
  public String partTwo() throws IOException {
    return "-";
  }

  private long countChar(final String s, final char c) {
    return s.chars().filter(item -> item == c).count();
  }
}
