package se.augustocesar.day05

import se.augustocesar.Task
import java.util.*
import kotlin.collections.ArrayDeque
import kotlin.streams.toList

class Day05 : Task() {
    override fun partOne(): String {
        return run(readInput(), false)
    }

    override fun partTwo(): String {
        return run(readInput(), true)
    }
}

class Move(val amount: Int, val from: Int, val to: Int)

private fun run(input: String, bulkMove: Boolean): String {
    val moves = readMoves(input)
    val stacks = readStacks(input)

    return String(applyMoves(stacks, moves, bulkMove).map { it.peek() }.toCharArray())
}

private fun applyMoves(stacks: List<Stack<Char>>, moves: List<Move>, bulkMove: Boolean): List<Stack<Char>> {
    for (move in moves) {
        if (bulkMove) {
            val craneQueue = ArrayDeque<Char>()
            repeat(move.amount) {
                craneQueue.addLast(stacks[move.from].pop())
            }

            while(!craneQueue.isEmpty()) {
                stacks[move.to].push(craneQueue.removeLast())
            }
        } else {
            repeat(move.amount) {
                val item = stacks[move.from].pop()
                stacks[move.to].push(item)
            }
        }
    }

    return stacks
}

private fun readStacks(input: String): List<Stack<Char>> {
    val lines = input.lines()
    val ignoreChars = listOf(' '.code, '['.code, ']'.code)
    val stacks = arrayListOf<Stack<Char>>()

    for (line in lines) {
        if (line.startsWith(" 1")) {
            break
        }

        for ((i, char) in line.chars().toList().withIndex()) {
            if (ignoreChars.contains(char)) {
                continue
            }

            val stackIndex = ((i - 1) / 4)
            if (stackIndex + 1 > stacks.size) {
                repeat(stackIndex + 1 - stacks.size) {
                    stacks.add(Stack())
                }
            }

            stacks[stackIndex].push(char.toChar())
        }
    }

    stacks.forEach { it.reverse() }
    return stacks
}

private fun readMoves(input: String): List<Move> {
    val moves = arrayListOf<Move>()
    Regex("move\\s(\\d+)\\sfrom\\s(\\d+)\\sto\\s(\\d+)").findAll(input).forEach { res ->
        val groups = res.destructured.toList().map { it.toInt() }
        moves.add(Move(groups[0], groups[1] - 1, groups[2] - 1))
    }

    return moves
}
