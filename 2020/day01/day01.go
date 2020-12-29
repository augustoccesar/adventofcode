package main

import (
	"fmt"
	"sort"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

func partOne() {
	report := getReport()
	lookupTable := make(map[int]int)

	for _, item := range report {
		lookupTable[2020-item] = item
	}

	for _, item := range report {
		if value, ok := lookupTable[item]; ok {
			fmt.Printf("Part One: %d\n", item*value)
			break
		}
	}
}

func partTwo() {
	report := getReport()
	sort.Ints(report)

	maximumThird := 2020 - (report[0] + report[1])

	possibleNumbers := []int{}
	for _, item := range report {
		if item <= maximumThird {
			possibleNumbers = append(possibleNumbers, item)
		}
	}

	validPins := false
	pin1 := 0
	pin2 := 1
	pin3 := len(possibleNumbers) - 1

	for !validPins {
		pinsResult := possibleNumbers[pin1] + possibleNumbers[pin2] + possibleNumbers[pin3]
		if pinsResult == 2020 {
			validPins = true
			continue
		}

		if pinsResult < 2020 {
			pin1++
			pin2++
			continue
		}

		if pinsResult > 2020 {
			pin3--
			continue
		}
	}

	fmt.Printf(
		"Part Two: %d\n",
		possibleNumbers[pin1]*possibleNumbers[pin2]*possibleNumbers[pin3],
	)
}

func getReport() []int {
	input := utils.ReadFile("./input.txt")

	report := strings.Split(string(input), "\n")
	intReport := make([]int, len(report))

	for i, item := range report {
		itemInt := utils.Atoi(item)

		intReport[i] = itemInt
	}

	return intReport
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
