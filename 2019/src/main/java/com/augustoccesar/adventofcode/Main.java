package com.augustoccesar.adventofcode;

import com.augustoccesar.adventofcode.day01.Day01;
import com.augustoccesar.adventofcode.day02.Day02;
import com.augustoccesar.adventofcode.day03.Day03;
import com.augustoccesar.adventofcode.day04.Day04;

import java.util.Map;

public class Main {
    public static void main(String[] args) {
        Map<String, BaseDay> days = Map.of(
                "01", new Day01(),
                "02", new Day02(),
                "03", new Day03(),
                "04", new Day04()
        );

        if (args.length < 1) {
            System.err.println("No Day arg passed.");
            System.exit(1);
        }

        String dayString = args[0];
        if (dayString.length() < 2) {
            dayString = "0" + dayString;
        }

        final BaseDay day = days.get(dayString);

        if (day == null) {
            System.err.println("Day not found.");
            System.exit(1);
        }

        day.run();
    }
}
