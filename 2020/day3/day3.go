package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
}

func buildMap() [][]rune {
	input := readInput()
	rows := strings.Split(input, "\n")
	tMap := make([][]rune, len(rows))

	for rowIdx, row := range rows {
		tMap[rowIdx] = []rune(row)
	}

	return tMap
}

func step(tmap [][]rune, rowIdx int, colIdx int, stepRight int, stepDown int) (newRowIdx int, newColIdx int) {
	rowLen := len(tmap[0])

	newColIdx = colIdx + stepRight
	newRowIdx = rowIdx + stepDown

	if newColIdx >= rowLen {
		newColIdx = newColIdx - rowLen
	}

	return newRowIdx, newColIdx
}

func countTrees(tMap [][]rune, stepsRight int, stepsDown int) int {
	trees := 0
	rowIdx := 0
	colIdx := 0

	for rowIdx < len(tMap)-1 {
		rowIdx, colIdx = step(tMap, rowIdx, colIdx, stepsRight, stepsDown)
		gridObj := string(tMap[rowIdx][colIdx])

		// fmt.Printf("%d, %d = %s\n", rowIdx, colIdx, gridObj)

		if gridObj == "#" {
			trees++
		}
	}

	return trees
}

var slopes [][]int = [][]int{
	[]int{1, 1},
	[]int{3, 1},
	[]int{5, 1},
	[]int{7, 1},
	[]int{1, 2},
}

var tMap = buildMap()

func partOne() {
	fmt.Printf("Part One: %d\n", countTrees(tMap, 3, 1))
}

func partTwo() {
	treesSlopes := make([]int, len(slopes))

	for i, slopeData := range slopes {
		stepRight, stepDown := slopeData[0], slopeData[1]

		treesSlopes[i] = countTrees(tMap, stepRight, stepDown)
	}

	result := 1
	for _, trees := range treesSlopes {
		result = result * trees
	}

	fmt.Printf("Part Two: %d\n", result)
}

func main() {
	partOne()
	partTwo()
}
