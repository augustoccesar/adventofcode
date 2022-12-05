package se.augustocesar.day02

import se.augustocesar.Task

class Day02 : Task() {
    override fun partOne(): String {
        val input = readInput()
        var score = 0

        input.lines().forEach {
            val playsRaw = it.split(" ")
            val opponentPlay = Play.fromRepresentation(playsRaw[0])
            val myPlay = Play.fromRepresentation(playsRaw[1])

            val outcome = myPlay.outcome(opponentPlay)

            score += outcome.points + myPlay.points
        }

        return score.toString()
    }

    override fun partTwo(): String {
        return "-"
    }
}

enum class Play(val points: Int) {
    ROCK(1),
    PAPER(2),
    SCISSOR(3);

    fun outcome(other: Play): Outcome {
        when(this) {
            ROCK -> {
                return when(other) {
                    ROCK -> Outcome.TIE
                    PAPER -> Outcome.LOSS
                    SCISSOR -> Outcome.WIN
                }
            }
            PAPER -> {
                return when(other) {
                    ROCK -> Outcome.WIN
                    PAPER -> Outcome.TIE
                    SCISSOR -> Outcome.LOSS
                }
            }
            SCISSOR -> {
                return when(other) {
                    ROCK -> Outcome.LOSS
                    PAPER -> Outcome.WIN
                    SCISSOR -> Outcome.TIE
                }
            }
        }
    }

    companion object {
        fun fromRepresentation(representation: String): Play {
            return when (representation) {
                "A", "X" -> ROCK
                "B", "Y" -> PAPER
                "C", "Z" -> SCISSOR
                else -> throw RuntimeException("Invalid representation")
            }
        }
    }
}

enum class Outcome(val points: Int) {
    WIN(6),
    LOSS(0),
    TIE(3)
}
