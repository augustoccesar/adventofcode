package day03

import (
	"strconv"
	"strings"
)

type Day03 struct{}

func (d *Day03) InputFileName() string { return "input" }

func (d *Day03) PartOne(input string) string {
	tMap := buildMap(input)
	return strconv.Itoa(countTrees(tMap, 3, 1))
}

func (d *Day03) PartTwo(input string) string {
	tMap := buildMap(input)
	treesSlopes := make([]int, len(slopes))

	for i, slopeData := range slopes {
		stepRight, stepDown := slopeData[0], slopeData[1]

		treesSlopes[i] = countTrees(tMap, stepRight, stepDown)
	}

	result := 1
	for _, trees := range treesSlopes {
		result = result * trees
	}

	return strconv.Itoa(result)
}

func buildMap(input string) [][]rune {
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
