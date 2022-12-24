package se.augustocesar.day10

import se.augustocesar.Task

class Day10 : Task() {
    override fun partOne(): String {
        val cycleStrengths = hashMapOf(20 to 0, 60 to 0, 100 to 0, 140 to 0, 180 to 0, 220 to 0)
        val commDevice = CommDevice()
        commDevice.loadProgram(readInput())

        while (cycleStrengths.values.any { it == 0 }) {
            commDevice.runCycle()
            if (cycleStrengths[commDevice.cycle] != null) {
                cycleStrengths[commDevice.cycle] = commDevice.register
            }
        }

        return cycleStrengths.entries.sumOf { (k, v) -> k * v }.toString()
    }

    override fun partTwo(): String {
        return "-"
    }
}

class CommDevice {
    var register: Int = 1
    var cycle: Int = 0

    private var currInstruction: Instruction? = null
    private var instructions: ArrayDeque<Instruction> = ArrayDeque()

    fun loadProgram(program: String) {
        program.lines().forEach { line ->
            val tokens = line.split(" ")
            val instruction = Instruction(
                    InstructionType.fromRepr(tokens[0]),
                    tokens.getOrNull(1)?.toInt(),
                    null
            )

            instructions.add(instruction)
        }
    }

    fun runCycle() {
        if (instructions.isEmpty()) {
            throw RuntimeException("No instruction to run")
        }

        cycle++

        if (currInstruction != null && currInstruction!!.isFinished(cycle)) {
            when (currInstruction!!.type) {
                InstructionType.NOOP -> Unit
                InstructionType.ADDX -> register += currInstruction!!.arg!!
            }

            currInstruction = null
        }

        if (currInstruction == null) {
            currInstruction = instructions.removeFirst()
            currInstruction!!.startedAtCycle = cycle
        }
    }
}

enum class InstructionType(val cyclesTime: Int) {
    NOOP(1),
    ADDX(2);

    companion object {
        fun fromRepr(repr: String): InstructionType {
            return when (repr.lowercase()) {
                "noop" -> NOOP
                "addx" -> ADDX
                else -> throw RuntimeException("Unsupported instruction: $repr")
            }
        }
    }
}

class Instruction(val type: InstructionType, var arg: Int?, var startedAtCycle: Int?) {
    fun isFinished(currentCycle: Int): Boolean {
        return (currentCycle - startedAtCycle!!) == type.cyclesTime
    }
}
