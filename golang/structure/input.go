package structure

import (
	"fmt"
	"os"
	"strconv"
)

func ReadDefaultInput(day Day) string {
	return ReadInput(day, "")
}

func ReadInput(day Day, inputName string) string {
	var inputPath string
	if inputName == "" {
		inputPath = fmt.Sprintf(
			"../inputs/%d_%02s.txt",
			day.Year(),
			strconv.Itoa(day.Day()),
		)
	} else {
		inputPath = fmt.Sprintf(
			"../inputs/%d_%02s_%s.txt",
			day.Year(),
			strconv.Itoa(day.Day()),
			inputName,
		)
	}

	inputData, err := os.ReadFile(inputPath)
	if err != nil {
		panic(fmt.Sprintf("Failed to read input file: %+v", err))
	}

	return string(inputData)
}
