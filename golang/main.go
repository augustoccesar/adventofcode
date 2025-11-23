package main

import (
	"fmt"
	"os"
	"strconv"

	"com.github/augustoccesar/adventofcode/golang/structure"
	// CODEGEN:target_import
)

type DayMapKey struct {
	year int
	day  int
}

var daysMap = map[DayMapKey]structure.Day{
	// CODEGEN:target_dict
}

func main() {
	yearInput, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic(fmt.Sprintf("Invalid format for year: %+v", os.Args[1]))
	}

	dayInput, err := strconv.Atoi(os.Args[2])
	if err != nil {
		panic(fmt.Sprintf("Invalid format for day: %+v", os.Args[2]))
	}

	day, dayFound := daysMap[DayMapKey{yearInput, dayInput}]

	if !dayFound {
		panic(fmt.Sprintf("Day %d for year %d not found", dayInput, yearInput))
	}

	fmt.Printf("Part one: %s\n", day.PartOne())
	fmt.Printf("Part two: %s\n", day.PartOne())
}
