package main

import (
	"fmt"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

// row, col
var directionModifiers = [][]int{
	{0, -1},  // Left
	{-1, -1}, // Up Left
	{-1, 0},  // Up
	{-1, 1},  // Up Right
	{0, 1},   // Right
	{1, 1},   // Down Right
	{1, 0},   // Down
	{1, -1},  // Down Left
}

func buildMap() [][]string {
	input := utils.ReadFile("./input.txt")

	rows := strings.Split(input, "\n")
	sMap := make([][]string, len(rows))

	for i, rowData := range rows {
		row := strings.Split(rowData, "")
		sMap[i] = row
	}

	return sMap
}

func getLineOfSigth(sMap [][]string, rowIdx int, colIdx int, sightLen int) []string {
	maxRow := len(sMap) - 1
	maxCol := len(sMap[0]) - 1
	inSight := []string{}

	for _, modifiers := range directionModifiers {
		foundSeat := ""
		reachedEnd := false
		spotsChecked := 0
		currRow, currCol := rowIdx, colIdx

		// Continue to look further if:
		//     - Haven't fount a seat yet
		//     - AND haven't reached the end of the direction
		//     - AND (have unlimited sight OR haven't reached the sight limit)
		for foundSeat == "" && !reachedEnd && (sightLen == -1 || spotsChecked < sightLen) {
			currRow, currCol = currRow+modifiers[0], currCol+modifiers[1]

			if currRow > maxRow || currCol > maxCol || currRow < 0 || currCol < 0 {
				reachedEnd = true
				continue
			}

			item := sMap[currRow][currCol]
			if item == "L" || item == "#" {
				foundSeat = item
			}

			spotsChecked++
		}

		if foundSeat != "" {
			inSight = append(inSight, foundSeat)
		}
	}

	return inSight
}

func runIterations(sMap *[][]string, maxOccupied int, sightLen int) {
	hadChanges := true
	for hadChanges {
		nextMap := utils.MatrixDeepCopy(*sMap)

		changes := 0
		for rowIdx, row := range *sMap {
			for colIdx, spot := range row {
				if spot == "." {
					continue
				}

				spotsOnSight := getLineOfSigth(*sMap, rowIdx, colIdx, sightLen)
				itemsCount := utils.SliceCount(spotsOnSight)

				if spot == "L" && itemsCount["#"] == 0 {
					nextMap[rowIdx][colIdx] = "#"
					changes++
					continue
				}

				if spot == "#" && itemsCount["#"] >= maxOccupied {
					nextMap[rowIdx][colIdx] = "L"
					changes++
					continue
				}
			}
		}

		*sMap = nextMap
		if changes == 0 {
			hadChanges = false
		}
	}
}

func partOne() {
	sMap := buildMap()

	runIterations(&sMap, 4, 1)

	fmt.Printf("Part One: %d\n", utils.MatrixCount(sMap)["#"])
}

func partTwo() {
	sMap := buildMap()

	runIterations(&sMap, 5, -1)

	fmt.Printf("Part Two: %d\n", utils.MatrixCount(sMap)["#"])
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
