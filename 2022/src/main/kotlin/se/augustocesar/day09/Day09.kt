package se.augustocesar.day09

import se.augustocesar.Task
import kotlin.math.abs

class Day09 : Task() {
    override fun partOne(): String {
        val rope = Rope()

        readInput().lines().forEach { line ->
            val tokens = line.split(" ")
            val direction = Direction.fromRepresentation(tokens[0])
            val steps = tokens[1].toInt()

            repeat(steps) { rope.moveHead(direction) }
        }

        return rope.tailTracking.size.toString()
    }

    override fun partTwo(): String {
        return "-"
    }
}

class Rope {
    private var head: Point2D = Point2D(0, 0)
    private var tail: Point2D = Point2D(0, 0)
    val tailTracking: HashSet<String> = hashSetOf(tail.key())

    fun moveHead(direction: Direction) {
        head = head.sum(direction.modifier)
        if (!head.isTouching(tail)) {
            moveTail()
        }
    }

    private fun moveTail() {
        val compDirection = tail.directionToFollow(head)

        if (compDirection.xAxis != null) {
            tail = tail.sum(compDirection.xAxis.modifier)
        }

        if (compDirection.yAxis != null) {
            tail = tail.sum(compDirection.yAxis.modifier)
        }

        tailTracking.add(tail.key())
    }
}

data class Point2D(val x: Int, val y: Int) {
    fun key(): String {
        return "$x,$y"
    }

    fun sum(other: Point2D): Point2D {
        return Point2D(x + other.x, y + other.y)
    }

    fun isTouching(other: Point2D): Boolean {
        val dx = abs(x - other.x)
        val dy = abs(y - other.y)

        return dx <= 1 && dy <= 1
    }

    fun directionToFollow(other: Point2D): CompositeDirection {
        val xAxis = if (x < other.x) {
            Direction.RIGHT
        } else if (x > other.x) {
            Direction.LEFT
        } else {
            null
        }

        val yAxis = if (y < other.y) {
            Direction.UP
        } else if (y > other.y) {
            Direction.DOWN
        } else {
            null
        }

        return CompositeDirection(xAxis, yAxis)
    }
}

data class CompositeDirection(val xAxis: Direction?, val yAxis: Direction?)

enum class Direction(val modifier: Point2D) {
    UP(Point2D(0, 1)),
    RIGHT(Point2D(1, 0)),
    DOWN(Point2D(0, -1)),
    LEFT(Point2D(-1, 0));

    companion object {
        fun fromRepresentation(repr: String): Direction {
            return when (repr.uppercase()) {
                "U" -> UP
                "R" -> RIGHT
                "D" -> DOWN
                "L" -> LEFT
                else -> throw RuntimeException("Invalid direction representation: $repr")
            }
        }
    }
}
