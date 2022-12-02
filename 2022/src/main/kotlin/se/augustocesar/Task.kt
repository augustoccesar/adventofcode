package se.augustocesar

abstract class Task {
    abstract fun partOne(): String
    abstract fun partTwo(): String

    protected fun readInput(suffix: String = "input"): String {
        val day = this.javaClass.name.split('.').last().lowercase()

        return this.javaClass.getResource("/inputs/${day}_${suffix}.txt")?.readText()
            ?: throw RuntimeException("Failed to load input")
    }
}
