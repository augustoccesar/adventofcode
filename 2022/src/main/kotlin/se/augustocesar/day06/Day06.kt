package se.augustocesar.day06

import se.augustocesar.Task
import kotlin.streams.toList

class Day06 : Task() {
    override fun partOne(): String {
        val chars = readInput().chars().toList()

        for (i in 3 until chars.size) {
            if (setOf(chars[i], chars[i - 1], chars[i - 2], chars[i - 3]).size == 4) {
                return (i + 1).toString()
            }
        }

        throw RuntimeException("Unreachable")
    }

    override fun partTwo(): String {
        val chars = readInput().chars().toList()

        for (i in 13 until chars.size) {
            val checkChars = HashSet<Int>()
            for (j in 0 until 14) {
                checkChars.add(chars[i - j])
            }

            if(checkChars.size == 14) {
                return (i + 1).toString()
            }
        }

        throw RuntimeException("Unreachable")
    }
}
