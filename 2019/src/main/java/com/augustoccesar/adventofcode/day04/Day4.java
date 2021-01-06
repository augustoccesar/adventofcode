package com.augustoccesar.adventofcode.day04;

import com.augustoccesar.adventofcode.BaseDay;
import com.augustoccesar.adventofcode.utils.Pair;
import com.augustoccesar.adventofcode.utils.Point2D;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day4 extends BaseDay {
    @Override
    public void partOne() throws IOException {
        final String[] input = this.readInput().strip().split("-");
        final int rangeStart = Integer.parseInt(input[0]);
        final int rangeEnd = Integer.parseInt(input[1]);

        AtomicInteger valid = new AtomicInteger(0);

        IntStream.rangeClosed(rangeStart, rangeEnd).forEach((trying) -> {
            if (isValid(trying)) {
                valid.incrementAndGet();
            }
        });

        System.out.println("Part One: " + valid.get());
    }

    @Override
    public void partTwo() throws IOException {
        System.out.println("Part Two: ");
    }

    private boolean isValid(int password) {
        List<Character> passChars = String.valueOf(password).chars().mapToObj((c) -> (char) c).collect(Collectors.toList());
        boolean hasDouble = false;

        for (int i = 0; i < passChars.size(); i++) {
            final int currentValue = Character.getNumericValue(passChars.get(i));

            if (i == 0) {
                continue;
            }

            int previousValue = Character.getNumericValue(passChars.get(i - 1));

            if (currentValue > previousValue) {
                continue;
            }

            if (currentValue == previousValue) {
                hasDouble = true;
                continue;
            }

            return false;
        }

        return hasDouble;
    }
}
