package se.augustocesar.day01

import se.augustocesar.Task

class Day01 : Task() {
    override fun partOne(): String {
        return caloriesPerElf(readInput()).maxOrNull().toString()
    }

    override fun partTwo(): String {
        return caloriesPerElf(readInput())
            .sortedDescending()
            .slice(0..2)
            .sum()
            .toString()
    }

    private fun caloriesPerElf(input: String): List<Int> {
        return input
            .split("\n\n")
            .map { group -> group.split('\n').map { it.toInt() } }
            .map { it.sum() }
    }
}
