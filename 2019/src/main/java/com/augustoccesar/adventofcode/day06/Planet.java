package com.augustoccesar.adventofcode.day06;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Builder
public class Planet {
  private String name;
  @Setter
  private Planet orbit;
}
