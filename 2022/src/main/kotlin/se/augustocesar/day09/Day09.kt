package se.augustocesar.day09

import se.augustocesar.Task
import kotlin.math.abs

class Day09 : Task() {
    override fun partOne(): String {
        val rope = Rope.withKnots(2)

        readInput().lines().forEach { line ->
            val tokens = line.split(" ")
            val direction = Direction.fromRepresentation(tokens[0])
            val steps = tokens[1].toInt()

            repeat(steps) { rope.moveHead(direction) }
        }

        return rope.tailTracking.size.toString()
    }

    override fun partTwo(): String {
        val rope = Rope.withKnots(10)

        readInput().lines().forEach { line ->
            val tokens = line.split(" ")
            val direction = Direction.fromRepresentation(tokens[0])
            val steps = tokens[1].toInt()

            repeat(steps) { rope.moveHead(direction) }
        }

        return rope.tailTracking.size.toString()
    }
}

class Knot(var next: Knot?, var previous: Knot?) {
    var position: Point2D = Point2D(0, 0)
}

class Rope private constructor(size: Int) {
    private val head = Knot(null, null)
    val tailTracking: HashSet<String> = hashSetOf(Point2D(0, 0).key())

    init {
        var current = head
        repeat(size - 1) {
            val knot = Knot(null, current)
            current.next = knot

            current = knot
        }
    }

    fun moveHead(direction: Direction) {
        head.position = head.position.sum(direction.modifier)

        var currKnot = head.next
        while (currKnot != null) {
            if (currKnot.position.isTouching(currKnot.previous!!.position)) {
                break
            }

            val compDirection = currKnot.position.directionToFollow(currKnot.previous!!.position)

            if (compDirection.xAxis != null) {
                currKnot.position = currKnot.position.sum(compDirection.xAxis.modifier)
            }

            if (compDirection.yAxis != null) {
                currKnot.position = currKnot.position.sum(compDirection.yAxis.modifier)
            }

            if(currKnot.next == null) {
                tailTracking.add(currKnot.position.key())
            }

            currKnot = currKnot.next
        }
    }

    companion object {
        fun withKnots(size: Int): Rope {
            return Rope(size)
        }
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
