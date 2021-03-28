package com.augustoccesar.adventofcode.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ListUtil {

  public static <T> List<T> shiftLeft(List<T> original, int amount) {
    List<T> target = new ArrayList<>(Collections.nCopies(original.size(), null));

    for (int i = 0; i < original.size(); i++) {
      T tmp = original.get(i);
      int targetIdx = i - amount;
      if (targetIdx < 0) {
        targetIdx = original.size() + targetIdx;
      }

      target.set(targetIdx, tmp);
    }

    return target;
  }
}
