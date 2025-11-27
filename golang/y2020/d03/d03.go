package d03

import (
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
)

type Day03 struct{}

func (d *Day03) Year() int { return 2020 }
func (d *Day03) Day() int  { return 3 }

func (d *Day03) PartOne() string {
	tMap := buildMap(structure.ReadDefaultInput(d))
	return strconv.Itoa(countTrees(tMap, 3, 1))
}

func (d *Day03) PartTwo() string {
	tMap := buildMap(structure.ReadDefaultInput(d))
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
	{1, 1},
	{3, 1},
	{5, 1},
	{7, 1},
	{1, 2},
}
