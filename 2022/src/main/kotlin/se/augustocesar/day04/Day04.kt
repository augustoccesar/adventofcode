package se.augustocesar.day04

import se.augustocesar.Task

class Day04 : Task() {
    override fun partOne(): String {
        var total = 0

        readInput().lines().forEach {line ->
            val sections = line
                .split(",")
                .map { section -> section.split("-").map { item -> item.toInt() } }
                .map { IntRange(it[0], it[1]) }

            if(sections[0].subtract(sections[1]).isEmpty() || sections[1].subtract(sections[0]).isEmpty()) {
                total++
            }
        }

        return total.toString()
    }

    override fun partTwo(): String {
        var total = 0

        readInput().lines().forEach {line ->
            val sections = line
                .split(",")
                .map { section -> section.split("-").map { item -> item.toInt() } }
                .map { IntRange(it[0], it[1]) }

            if(sections[0].intersect(sections[1]).isNotEmpty()) {
                total++
            }
        }

        return total.toString()
    }
}
