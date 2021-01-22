package day23

import (
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type Day23 struct{}

func (d *Day23) InputFileName() string { return "input" }

func (d *Day23) PartOne(input string) string {
	inputItems := strings.Split(input, "")
	cups := make([]int, len(inputItems))

	for i, item := range inputItems {
		cups[i] = utils.Atoi(item)
	}

	circle := playGame(cups, 100)

	result := strings.Builder{}
	for _, cup := range circle.Tail(1) {
		result.WriteString(strconv.Itoa(cup.Label))
	}

	return result.String()
}

func (d *Day23) PartTwo(input string) string {
	inputItems := strings.Split(input, "")
	cupsAmount := 1_000_000
	moves := 10_000_000

	cups := make([]int, cupsAmount)

	for i := 0; i < len(inputItems); i++ {
		cups[i] = utils.Atoi(inputItems[i])
	}
	maxCupLabel, _ := utils.SliceIntMaxMin(cups)

	for i, j := len(inputItems), maxCupLabel+1; i < cupsAmount; i, j = i+1, j+1 {
		cups[i] = j
	}

	circle := playGame(cups, moves)

	cupOne := circle.Find(1)
	resCups := []*Cup{cupOne.Next, cupOne.Next.Next}

	return strconv.Itoa(resCups[0].Label * resCups[1].Label)
}

func playGame(cupLabels []int, moves int) *Circle {
	maxCupLabel, minCupLabel := utils.SliceIntMaxMin(cupLabels)

	circle := NewCircle(cupLabels)

	currCup := circle.Find(cupLabels[0])
	for i := 0; i < moves; i++ {
		// The crab picks up the three cups that are immediately clockwise of the current cup.
		// They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
		// removedCups := circle.PopFromIdx(currCupIdx+1, 3)
		removedCups := circle.PopFromLabel(currCup.Next.Label, 3)

		// The crab selects a destination cup: the cup with a label equal to the current
		// cup's label minus one.
		destinationCupLabel := currCup.Label
		var destinationCup *Cup

		// If this would select one of the cups that was just picked up,
		// the crab will keep subtracting one until it finds a cup that wasn't just picked up.
		for destinationCup == nil {
			destinationCupLabel--
			destinationCup = circle.Find(destinationCupLabel)

			// If at any point in this process the value goes below the lowest value on any cup's label,
			// it wraps around to the highest value on any cup's label instead.
			if destinationCupLabel < minCupLabel {
				destinationCupLabel = maxCupLabel + 1 // compensate for decrement on start of loop
			}
		}

		// The crab places the cups it just picked up so that they are immediately clockwise of the destination cup.
		// They keep the same order as when they were picked up.
		circle.InsertAfterLabel(destinationCup.Label, removedCups)

		currCup = currCup.Next
	}

	return circle
}

type Cup struct {
	Label    int
	Next     *Cup
	Previous *Cup
}

type Circle struct {
	cupsMap map[int]*Cup
	Head    *Cup
}

func NewCircle(cups []int) *Circle {
	m := make(map[int]*Cup, len(cups))
	for i := 0; i < len(cups); i++ {
		m[cups[i]] = &Cup{Label: cups[i]}
	}

	for i := 0; i < len(cups); i++ {
		nextIdx := i + 1
		previousIdx := i - 1

		if previousIdx < 0 {
			previousIdx = len(cups) + previousIdx
		}

		if nextIdx > len(cups)-1 {
			nextIdx = 0
		}

		m[cups[i]].Next = m[cups[nextIdx]]
		m[cups[i]].Previous = m[cups[previousIdx]]
	}

	return &Circle{Head: m[cups[0]], cupsMap: m}
}

// Inclusive
func (c *Circle) PopFromLabel(label, amount int) []*Cup {
	popped := make([]*Cup, amount)
	startCup := c.cupsMap[label]

	headAffected := false
	endCup := startCup
	for i := 0; i < amount; i++ {
		popped[i] = endCup
		headAffected = headAffected || endCup == c.Head

		// If there are still loops to go, move the end cup
		if i < amount-1 {
			endCup = endCup.Next
		}
	}

	// Remove the cups from the lookup map
	for i := 0; i < len(popped); i++ {
		delete(c.cupsMap, popped[i].Label)
	}

	if headAffected {
		c.Head = startCup.Previous
	}

	// Fix the circle
	startCup.Previous.Next = endCup.Next
	endCup.Next.Previous = startCup.Previous

	// Close the circle of the removed ones
	if len(popped) > 1 {
		popped[0].Previous = popped[len(popped)-1]
		popped[len(popped)-1].Next = popped[0]
	} else {
		popped[0].Next = nil
		popped[0].Previous = nil
	}

	return popped
}

func (c *Circle) InsertAfterLabel(label int, cups []*Cup) {
	startCup := c.cupsMap[label]
	endCup := startCup.Next

	startCup.Next = cups[0]
	cups[0].Previous = startCup

	endCup.Previous = cups[len(cups)-1]
	cups[len(cups)-1].Next = endCup

	for i := 0; i < len(cups); i++ {
		c.cupsMap[cups[i].Label] = cups[i]
	}
}

func (c *Circle) Find(label int) *Cup {
	return c.cupsMap[label]
}

func (c *Circle) Tail(fromLabel int) []*Cup {
	tail := make([]*Cup, len(c.cupsMap)-1)

	looped := false
	i := 0
	baseCup := c.Find(fromLabel)
	cup := baseCup.Next
	for !looped {
		tail[i] = cup
		cup = cup.Next
		if cup == baseCup {
			looped = true
		}

		i++
	}

	return tail
}
