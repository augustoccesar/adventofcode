package main

import (
	"fmt"
	"math"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

func partOne() {
	input := utils.ReadFile("./input.txt")
	data := strings.Split(input, "\n")

	earliestOnPort := utils.Atoi(data[0])
	busIDs := strings.Split(data[1], ",")
	shortestID, shortestTime := math.MaxInt64, math.MaxInt64

	for _, time := range busIDs {
		if time == "x" {
			continue
		}

		iTime := utils.Atoi(time)
		mult := earliestOnPort / iTime
		nextTime := -1
		for nextTime == -1 {
			res := iTime * mult
			if res >= earliestOnPort {
				nextTime = res
			}
			mult++
		}

		if nextTime < shortestTime {
			shortestID = iTime
			shortestTime = nextTime
		}
	}

	res := (shortestTime - earliestOnPort) * shortestID
	fmt.Printf("Part One: %d\n", res)
}

func partTwo() {
	input := strings.Split(utils.ReadFile("./input.txt"), "\n")[1]
	busIDs := strings.Split(input, ",")

	time := 0
	inc := utils.Atoi(busIDs[0]) // Initial increment is by the first bus time
	for i := 1; i < len(busIDs); i++ {
		if busIDs[i] == "x" {
			continue
		}

		newTime := utils.Atoi(busIDs[i])
		for true {
			time += inc
			if (time+i)%newTime == 0 {
				// Include this bus on the increment from now
				// on (it will repeat all the ones until this point)
				// fizz-buzz style
				inc *= newTime
				break
			}
		}
	}

	fmt.Printf("Part Two: %d\n", time)
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
