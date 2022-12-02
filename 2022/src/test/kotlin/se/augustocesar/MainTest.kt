package se.augustocesar

import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals

class MainTest {
    private val expectedResultsMap: HashMap<String, List<String>> = hashMapOf()

    @BeforeTest
    fun before() {
        val expectedResults = this.javaClass.getResource("/expected_results")?.readText()
            ?: throw RuntimeException("Failed to load expected results resource")

        expectedResults.split('\n').map { it.split(';') }.forEach { row ->
            expectedResultsMap[row[0]] = listOf(row[1], row[2])
        }
    }

    @Test
    fun testResults() {
        daysMap.forEach { (day, task) ->
            val expectedValues = expectedResultsMap[day]
                ?: throw RuntimeException("Expected results not found for day `${day}`")

            assertEquals(expectedValues[0], task.partOne())
            assertEquals(expectedValues[1], task.partTwo())
        }
    }
}
