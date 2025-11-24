package y2020

import (
	"sort"
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
)

type Day01 struct{}

func (d *Day01) Year() int { return 2020 }
func (d *Day01) Day() int  { return 1 }

func (d *Day01) PartOne() string {
	var result int
	report := getReport(structure.ReadDefaultInput(d))
	lookupTable := make(map[int]int)

	for _, item := range report {
		lookupTable[2020-item] = item
	}

	for _, item := range report {
		if value, ok := lookupTable[item]; ok {
			result = item * value
			break
		}
	}

	return strconv.Itoa(result)
}

func (d *Day01) PartTwo() string {
	var result int
	report := getReport(structure.ReadDefaultInput(d))
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

	result = possibleNumbers[pin1] * possibleNumbers[pin2] * possibleNumbers[pin3]

	return strconv.Itoa(result)
}

func getReport(input string) []int {
	report := strings.Split(string(input), "\n")
	intReport := make([]int, len(report))

	for i, item := range report {
		itemInt, err := strconv.Atoi(item)
		if err != nil {
			panic(err)
		}

		intReport[i] = itemInt
	}

	return intReport
}
