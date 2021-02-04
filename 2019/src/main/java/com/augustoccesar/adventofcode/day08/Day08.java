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
    String data = this.readInput();
    int width = 25;
    int height = 6;

    int layerDataSize = width * height;
    int layersCount = data.length() / layerDataSize;
    char[][][] image = new char[layerDataSize][height][width];

    for (int i = 0, j = 0; i < data.length(); i += layerDataSize, j++) {
      String layer = data.substring(i, Math.min(i + layerDataSize, data.length()));

      for (int k = 0; k < layer.length(); k++) {
        int row = k / width;
        int col = k % width;

        image[j][row][col] = layer.charAt(k);
      }
    }

    char[][] overlaidImage = new char[height][width];
    for (int row = 0; row < height; row++) {
      for (int col = 0; col < width; col++) {
        char color = '-';
        for (int i = 0; i < layersCount; i++) {
          char[][] layer = image[i];
          if (layer[row][col] == '2') {
            continue;
          }

          color = layer[row][col];
          break;
        }

        overlaidImage[row][col] = color;
      }
    }

    // This will print the result to the console
    // printImage(overlaidImage);

    return "ruzbp"; // Have to be raw. Need to check the print of the function above on the console
  }

  private long countChar(final String s, final char c) {
    return s.chars().filter(item -> item == c).count();
  }

  private void printImage(char[][] image) {
    for (int row = 0; row < image.length; row++) {
      for (int col = 0; col < image[0].length; col++) {
        char c = image[row][col];
        if (c == '1') {
          System.out.print(c);
        } else {
          System.out.print(" ");
        }
      }
      System.out.println();
    }
  }
}
