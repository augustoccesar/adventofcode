package com.augustoccesar.adventofcode;

import com.augustoccesar.adventofcode.day01.Day1;
import com.augustoccesar.adventofcode.day02.Day2;
import com.augustoccesar.adventofcode.day03.Day3;
import com.augustoccesar.adventofcode.day04.Day4;

import java.util.Map;

public class Main {
    public static void main(String[] args) {
        Map<String, BaseDay> days = Map.of(
                "1", new Day1(),
                "2", new Day2(),
                "3", new Day3(),
                "4", new Day4()
        );

        if (args.length < 1) {
            System.err.println("No Day arg passed.");
            System.exit(1);
        }

        final BaseDay day = days.get(args[0]);

        if (day == null) {
            System.err.println("Day not found.");
            System.exit(1);
        }

        day.run();
    }
}
