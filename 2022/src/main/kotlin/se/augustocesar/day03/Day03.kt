package se.augustocesar.day03

import se.augustocesar.Task
import kotlin.streams.toList

class Day03 : Task() {
    override fun partOne(): String {
        var total = 0

        readInput().lines().forEach {
            val found = hashMapOf<Int, Boolean>()
            val items = it.chars().toList()

            for ((i, item) in items.withIndex()) {
                val itemPrio = charASCIIToPrio(item)

                if (i < items.size / 2) {
                    found[itemPrio] = true
                    continue
                }

                if (found.containsKey(itemPrio)) {
                    total += itemPrio
                    break
                }
            }
        }

        return total.toString()
    }

    override fun partTwo(): String {
        return "-"
    }
}

private fun charASCIIToPrio(charASCII: Int): Int {
    return if(charASCII > 96) {
        charASCII - 96
    } else {
        charASCII - 38
    }
}
