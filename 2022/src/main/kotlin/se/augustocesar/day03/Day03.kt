package se.augustocesar.day03

import se.augustocesar.Task
import kotlin.streams.toList

class Day03 : Task() {
    override fun partOne(): String {
        var total = 0

        readInput().lines().forEach {
            val items = it.chars().toList()

            val firstHalf = items.subList(0, items.size / 2).toSet()
            val secondHalf = items.subList(items.size / 2, items.size).toSet()

            val item = firstHalf.intersect(secondHalf).first()
            total += charASCIIToPrio(item)
        }

        return total.toString()
    }

    override fun partTwo(): String {
        val rucksacks = readInput().lines()
        var total = 0

        for (i in rucksacks.indices step 3) {
            val res = rucksacks[i].toSet()
                .intersect(rucksacks[i + 1].toSet())
                .intersect(rucksacks[i + 2].toSet())
                .first()
                .code

            total += charASCIIToPrio(res)
        }

        return total.toString()
    }
}

private fun charASCIIToPrio(charASCII: Int): Int {
    return when(charASCII) {
        in 97..122 -> charASCII - 96 // a-z
        in 65..90 -> charASCII - 38 // A-Z
        else -> throw RuntimeException("Unsupported char ASCII code")
    }
}
