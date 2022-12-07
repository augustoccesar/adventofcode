package se.augustocesar.day05

import se.augustocesar.Task
import java.util.*
import kotlin.streams.toList

class Day05 : Task() {
    override fun partOne(): String {
        val input = readInput()
        val moves = readMoves(input)
        val stacks = readStacks(input)

        return String(applyMoves(stacks, moves).map { it.peek() }.toCharArray())
    }

    override fun partTwo(): String {
        return "-"
    }
}

class Move(val amount: Int, val from: Int, val to: Int)

private fun applyMoves(stacks: List<Stack<Char>>, moves: List<Move>): List<Stack<Char>> {
    for (move in moves) {
        repeat(move.amount) {
            val item = stacks[move.from].pop()
            stacks[move.to].push(item)
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

//private fun processInput(input: String): Data {
//    val lines = input.lines()
//    val ignoreChars = listOf(' '.code, '['.code, ']'.code)
//    val stacks = arrayListOf<Stack<Char>>()
//
//    for (line in lines) {
//        if (line.startsWith(" 1")) {
//            break
//        }
//
//        for ((i, char) in line.chars().toList().withIndex()) {
//            if (ignoreChars.contains(char)) {
//                continue
//            }
//
//            val stackIndex = ((i - 1) / 4)
//            if (stackIndex + 1 > stacks.size) {
//                repeat(stackIndex + 1 - stacks.size) {
//                    stacks.add(Stack())
//                }
//            }
//
//            stacks[stackIndex].push(char.toChar())
//        }
//    }
//
//    val moves = arrayListOf<Move>()
//    Regex("move\\s(\\d+)\\sfrom\\s(\\d+)\\sto\\s(\\d+)").findAll(input).forEach { res ->
//        val groups = res.destructured.toList().map { it.toInt() }
//        moves.add(Move(groups[0], groups[1] - 1, groups[2] - 1))
//    }
//
//    stacks.forEach { it.reverse() }
//    return Data(stacks, moves)
//}
