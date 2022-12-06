package se.augustocesar.day04

import se.augustocesar.Task

class Day04 : Task() {
    override fun partOne(): String {
        return run(readInput(), true).toString()
    }

    override fun partTwo(): String {
        return run(readInput(), false).toString()
    }
}

private fun run(input: String, fullIntersectOnly: Boolean): Int {
    var total = 0

    input.lines().forEach { line ->
        val sections = lineIntoRanges(line)

        if (checkIntersect(sections[0], sections[1], fullIntersectOnly)) {
            total++
        }
    }

    return total
}

private fun checkIntersect(left: IntRange, right: IntRange, fullOnly: Boolean): Boolean {
    return when (fullOnly) {
        true -> left.subtract(right).isEmpty() || right.subtract(left).isEmpty()
        false -> left.intersect(right).isNotEmpty()
    }
}

private fun lineIntoRanges(line: String): List<IntRange> {
    return line
        .split(",")
        .map { section -> section.split("-").map { item -> item.toInt() } }
        .map { IntRange(it[0], it[1]) }
}
