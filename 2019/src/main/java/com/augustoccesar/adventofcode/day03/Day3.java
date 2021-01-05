package com.augustoccesar.adventofcode.day03;

import com.augustoccesar.adventofcode.BaseDay;
import com.augustoccesar.adventofcode.utils.Point2D;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Day3 extends BaseDay {
    @Override
    public void partOne() throws IOException {
        List<String> input = this.readInput().lines().collect(Collectors.toList());
        List<HashMap<String, Point2D>> lines = new ArrayList<>();

        for (String in : input) {
            lines.add(generatePath(Point2D.on(0, 0), in));
        }

        int shortest = Integer.MAX_VALUE;
        List<Point2D> intersect = intersect(lines.get(0), lines.get(1));

        for (Point2D point : intersect) {
            final int distance = point.taxicab(Point2D.on(0, 0));
            if (distance < shortest) {
                shortest = distance;
            }
        }

        System.out.println("Part One: " + shortest);
    }

    @Override
    public void partTwo() throws IOException {
        System.out.println("Part Two: Not implemented");
    }

    private HashMap<String, Point2D> generatePath(final Point2D basePoint, final String pathCommands) {
        List<String> commands = List.of(pathCommands.split(","));
        HashMap<String, Point2D> path = new HashMap<>(commands.size());

        Point2D lastPoint = basePoint;
        for (String command : commands) {
            char direction = command.charAt(0);
            int amount = Integer.parseInt(command.substring(1));

            for (int i = 0; i < amount; i++) {
                switch (direction) {
                    case 'U' -> lastPoint = lastPoint.moveY(1);
                    case 'R' -> lastPoint = lastPoint.moveX(1);
                    case 'D' -> lastPoint = lastPoint.moveY(-1);
                    case 'L' -> lastPoint = lastPoint.moveX(-1);
                }

                path.put(lastPoint.id(), lastPoint);
            }
        }

        return path;
    }

    private List<Point2D> intersect(final HashMap<String, Point2D> pathOne, final HashMap<String, Point2D> pathTwo) {
        List<Point2D> intersection = new ArrayList<>();

        for (Map.Entry<String, Point2D> entry : pathOne.entrySet()) {
            if (pathTwo.get(entry.getValue().id()) != null) {
                intersection.add(entry.getValue());
            }
        }

        return intersection;
    }
}
