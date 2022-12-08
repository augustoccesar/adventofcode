package se.augustocesar.day06

import se.augustocesar.Task
import kotlin.streams.toList

class Day06 : Task() {
    override fun partOne(): String {
        return lookForMarker(readInput(), 4)?.toString().orEmpty()
    }

    override fun partTwo(): String {
        return lookForMarker(readInput(), 14)?.toString().orEmpty()
    }
}

private fun lookForMarker(message: String, size: Int): Int? {
    val chars = message.chars().toList()

    for (i in (size - 1) until chars.size) {
        val checkChars = HashSet<Int>()
        for (j in 0 until size) {
            checkChars.add(chars[i - j])
        }

        if(checkChars.size == size) {
            return i + 1
        }
    }

    return null
}
