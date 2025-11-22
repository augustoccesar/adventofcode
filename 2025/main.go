package main

import (
	"fmt"
	"os"
	// SETUP:target_import
)

var daysMap = map[string]Day{
	// SETUP:target_dict
}

type Day interface {
	PartOne(input string) string
	PartTwo(input string) string
}

func Run(day Day, inputPath string) {
	input, err := os.ReadFile(inputPath)
	if err != nil {
		panic(fmt.Sprintf("Failed to read input: %+v", err))
	}

	inputStr := string(input)

	fmt.Printf("Part One: %s\n", day.PartOne(inputStr))
	fmt.Printf("Part Two: %s\n", day.PartTwo(inputStr))
}

func main() {
	dayInput := os.Args[1]
	day, dayFound := daysMap[dayInput]

	if !dayFound {
		panic(fmt.Sprintf("Day %s not found", dayInput))
	}

	var inputName string
	if len(os.Args) < 3 || os.Args[2] == "" {
		inputName = fmt.Sprintf("%s_input", dayInput)
	} else {
		inputName = fmt.Sprintf("%s_%s", dayInput, os.Args[2])
	}

	inputPath := fmt.Sprintf("./inputs/%s.txt", inputName)

	fmt.Printf("Day: %s\n", dayInput)
	fmt.Printf("Input file: %s\n", inputPath)

	inputData, err := os.ReadFile(inputPath)
	if err != nil {
		panic(fmt.Sprintf("Failed to read input file: %+v", err))
	}

	inputDataStr := string(inputData)

	fmt.Println("---------------------------")
	fmt.Printf("Part one: %s\n", day.PartOne(string(inputDataStr)))
	fmt.Printf("Part two: %s\n", day.PartOne(string(inputDataStr)))
}
