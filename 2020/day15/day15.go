package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

func resolve(lastTurn int) int {
	mem := map[int][2]int{}
	input := []int{}
	for i, item := range strings.Split(utils.ReadFile("./input.txt"), ",") {
		iItem, _ := strconv.Atoi(item)
		input = append(input, iItem)

		mem[iItem] = [2]int{-1, i}
	}

	turn := len(input)
	lastSpoken := input[len(input)-1]

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

func partOne() {
	res := resolve(2020)
	fmt.Printf("Part One: %d\n", res)
}

func partTwo() {
	// Well... it works
	res := resolve(30000000)
	fmt.Printf("Part Two: %d\n", res)
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
