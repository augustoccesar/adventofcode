package se.augustocesar

import se.augustocesar.day01.Day01
//SETUP:target_import
import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import kotlin.system.measureTimeMillis

fun main(args: Array<String>) {
    if (args.size != 1) {
        throw RuntimeException("Invalid number of arguments")
    }

    val day = args[0].padStart(2, '0')
    val task = daysMap[day] ?: throw RuntimeException("Day '${day}' not found on map")

    executeTimed("Part One") { task.partOne() }
    executeTimed("Part Two") { task.partTwo() }
}

fun executeTimed(header: String, fn: () -> String) {
    val res: String
    val timeMillis = measureTimeMillis {
        res = fn()
    }

    val formattedTime = formatTime(timeMillis)
    println("$header: $res (took $formattedTime)")
}

fun formatTime(timeMillis: Long): String {
    return DateTimeFormatter.ofPattern("mm:ss.SSS")
        .withZone(ZoneId.of("UTC"))
        .format(Instant.ofEpochMilli(timeMillis))
}

val daysMap = mapOf<String, Task>(
    "01" to Day01(),
//SETUP:target_map
)
