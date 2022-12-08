package se.augustocesar.day08

import se.augustocesar.Task
import kotlin.streams.toList

class Day08 : Task() {
    override fun partOne(): String {
        val lines = readInput().lines()
        val grid = arrayListOf<List<Int>>()

        lines.forEach { line ->
            grid.add(line.chars().map { it.toChar().digitToInt() }.toList())
        }

        var visibleTrees = 0
        for (row in 0 until grid.size) {
            for (col in 0 until grid[0].size) {
                if (visible(row, col, grid)) {
                    visibleTrees++
                }
            }
        }

        return visibleTrees.toString()
    }

    override fun partTwo(): String {
        val lines = readInput().lines()
        val grid = arrayListOf<List<Int>>()

        lines.forEach { line ->
            grid.add(line.chars().map { it.toChar().digitToInt() }.toList())
        }

        var maxScenic = 0
        for (row in 0 until grid.size) {
            for (col in 0 until grid[0].size) {
                val score = scenicScore(row, col, grid)
                if (score > maxScenic) {
                    maxScenic = score
                }
            }
        }

        return maxScenic.toString()
    }
}

fun scenicScore(row: Int, col: Int, map: List<List<Int>>): Int {
    val size = map[row][col]

    var visibleLeft = 0
    var visibleRight = 0
    var visibleUp = 0
    var visibleDown = 0

    // Look left
    for (c in (col - 1) downTo 0) {
        if (map[row][c] < size) {
            visibleLeft++
        } else {
            visibleLeft++
            break
        }
    }

    // Look up
    for (r in (row - 1) downTo 0) {
        if (map[r][col] < size) {
            visibleUp++
        } else {
            visibleUp++
            break
        }
    }

    // Look right
    for (c in (col + 1) until map[0].size) {
        if (map[row][c] < size) {
            visibleRight++
        } else {
            visibleRight++
            break
        }
    }

    // Look down
    for (r in (row + 1) until map.size) {
        if (map[r][col] < size) {
            visibleDown++
        } else {
            visibleDown++
            break
        }
    }

    return visibleLeft * visibleRight * visibleUp * visibleDown
}

fun visible(row: Int, col: Int, map: List<List<Int>>): Boolean {
    if (row == 0 || row == map.size - 1) {
        return true
    }

    if (col == 0 || col == map[0].size - 1) {
        return true
    }

    val size = map[row][col]

    var visibleLeft = true
    var visibleRight = true
    var visibleUp = true
    var visibleDown = true

    // Look left
    for (c in (col - 1) downTo 0) {
        if (map[row][c] >= size) {
            visibleLeft = false
            break
        }
    }

    // Look up
    for (r in (row - 1) downTo 0) {
        if (map[r][col] >= size) {
            visibleUp = false
            break
        }
    }

    // Look right
    for (c in (col + 1) until map[0].size) {
        if (map[row][c] >= size) {
            visibleRight = false
            break
        }
    }

    // Look down
    for (r in (row + 1) until map.size) {
        if (map[r][col] >= size) {
            visibleDown = false
            break
        }
    }

    return listOf(visibleLeft, visibleUp, visibleRight, visibleDown).any { it }
}
