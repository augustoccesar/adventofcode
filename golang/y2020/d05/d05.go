package d05

import (
	"sort"
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
)

type Day05 struct{}

func (d *Day05) Year() int { return 2020 }
func (d *Day05) Day() int  { return 5 }

func (d *Day05) PartOne() string {
	highestID := -1

	for _, pass := range strings.Split(structure.ReadDefaultInput(d), "\n") {
		id := calculateBoardpassID(pass)
		if id > highestID {
			highestID = id
		}
	}

	return strconv.Itoa(highestID)
}

func (d *Day05) PartTwo() string {
	passIDs := []int{}

	for _, pass := range strings.Split(structure.ReadDefaultInput(d), "\n") {
		id := calculateBoardpassID(pass)
		passIDs = append(passIDs, id)
	}

	sort.Ints(passIDs)

	myPassID := -1
	for i, id := range passIDs {
		if passIDs[i+1] == id+1 {
			continue
		}

		myPassID = id + 1
		break
	}

	return strconv.Itoa(myPassID)
}

func getHighAndLow(low int, high int, command string) (int, int) {
	dist := high - low

	// No need to worry about the last loop or float divisions since the
	// division will be a floor of the result (last loop 1/2 will be 0 So low
	// and high will be the same number in the end).

	if command == "F" || command == "L" {
		return low, low + (dist / 2)
	}

	if command == "B" || command == "R" {
		return high - (dist / 2), high
	}

	return 0, 0
}

func calculateBoardpassID(boardpass string) int {
	chars := strings.Split(boardpass, "")

	lowRow := 0
	highRow := 127
	for i := 0; i < 7; i++ {
		lowRow, highRow = getHighAndLow(lowRow, highRow, chars[i])
	}

	lowCol := 0
	highCol := 7
	for i := 7; i < 10; i++ {
		lowCol, highCol = getHighAndLow(lowCol, highCol, chars[i])
	}

	return (lowRow * 8) + lowCol
}
