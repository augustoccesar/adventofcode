package com.augustoccesar.adventofcode.utils;

import static org.junit.Assert.assertEquals;

import java.util.List;
import org.junit.Test;

public class ListUtilTest {

  @Test
  public void shouldShiftListToLeft() {
    List<Integer> list = List.of(1, 2, 3);

    List<Integer> newList = ListUtil.shiftLeft(list, 1);

    assertEquals(List.of(2, 3, 1), newList);
  }
}
