package main

import (
	"fmt"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

func partOne() {
	input := utils.ReadFile("./input.txt")
	cups := []int{}
	for _, item := range strings.Split(input, "") {
		cups = append(cups, utils.Atoi(item))
	}
	maxCupLabel, minCupLabel := utils.SliceIntMaxMin(cups)

	circle := utils.NewSliceIntCircular(cups)

	currCupIdx := 0
	for i := 0; i < 100; i++ {
		currCupLabel := circle.Get(currCupIdx)

		// fmt.Printf("--- move %d ---\n", i+1)
		// fmt.Printf("Current Cup: %d\n", currCupLabel)
		// fmt.Printf("Cups: %+v\n", circle)

		// The crab picks up the three cups that are immediately clockwise of the current cup.
		// They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
		removedCups := circle.PopFromIdx(currCupIdx+1, 3)

		// The crab selects a destination cup: the cup with a label equal to the current
		// cup's label minus one.
		destinationCupLabel := currCupLabel
		destinationCupIdx := -1

		// If this would select one of the cups that was just picked up,
		// the crab will keep subtracting one until it finds a cup that wasn't just picked up.
		for destinationCupIdx == -1 {
			destinationCupLabel--
			destinationCupIdx = circle.Find(destinationCupLabel)

			// If at any point in this process the value goes below the lowest value on any cup's label,
			// it wraps around to the highest value on any cup's label instead.
			if destinationCupLabel < minCupLabel {
				destinationCupLabel = maxCupLabel + 1 // compensate for decrement on start of loop
			}
		}

		// fmt.Printf("Removed: %+v\n", removedCups)
		// fmt.Printf("Remaining: %+v\n", circle)
		// fmt.Printf("Destination: idx: %d, label: %d\n", destinationCupIdx, destinationCupLabel)

		// The crab places the cups it just picked up so that they are immediately clockwise of the destination cup.
		// They keep the same order as when they were picked up.
		circle.InsertAt(destinationCupIdx, removedCups)

		// fmt.Printf("New Cups: %+v\n\n", circle)

		// Update location of the current cup
		currCupIdx = circle.Find(currCupLabel)
		currCupIdx = (currCupIdx + 1) % circle.Len()
	}

	oneIdx := circle.Find(1)
	tail := circle.Tail(oneIdx)

	fmt.Printf("Part One: %s\n", strings.Join(utils.SliceIntToStr(tail), ""))
}

func partTwo() {

}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
