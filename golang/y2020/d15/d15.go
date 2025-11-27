package d15

import (
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
)

type Day15 struct{}

func (d *Day15) Year() int { return 2020 }
func (d *Day15) Day() int  { return 15 }

func (d *Day15) PartOne() string {
	return strconv.Itoa(resolve(structure.ReadDefaultInput(d), 2020))
}

func (d *Day15) PartTwo() string {
	// Well... it works
	return strconv.Itoa(resolve(structure.ReadDefaultInput(d), 30000000))
}

func resolve(input string, lastTurn int) int {
	mem := map[int][2]int{}
	inputIntArr := []int{}
	for i, item := range strings.Split(input, ",") {
		iItem, _ := strconv.Atoi(item)
		inputIntArr = append(inputIntArr, iItem)

		mem[iItem] = [2]int{-1, i}
	}

	turn := len(inputIntArr)
	lastSpoken := inputIntArr[len(inputIntArr)-1]

	for turn < lastTurn {
		say := 0
		if val, ok := mem[lastSpoken]; ok {
			if val[0] != -1 { // If it was not the first time the previous number was spoken
				say = val[1] - val[0]
			}
		}

		if val, ok := mem[say]; ok {
			mem[say] = [2]int{val[1], turn}
		} else {
			mem[say] = [2]int{-1, turn}
		}

		lastSpoken = say
		turn++
	}

	return lastSpoken
}
