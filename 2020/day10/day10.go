package day10

import (
	"sort"
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type Day10 struct{}

func (d *Day10) InputFileName() string { return "input" }

func (d *Day10) PartOne(input string) string {
	adapters := readIntSliceInput(input)
	sort.Ints(adapters)

	largestJolts := adapters[len(adapters)-1]
	smalledsJolts := adapters[0]
	lookup := map[int]bool{}

	for _, item := range adapters {
		lookup[item] = true
	}

	// 3 starts with 1 to compensate for the built-in adapter on the device
	// that is always 3 jolts more than the largest adapter
	diffs := map[int]int{1: 0, 2: 0, 3: 1}

	// Add the starting adapter diff to the outlet
	diffs[smalledsJolts]++

	for _, adapter := range adapters {
		if adapter == largestJolts {
			break
		}

		validAdapters := []int{}
		for i := 1; i <= 3; i++ {
			if _, ok := lookup[adapter+i]; ok {
				validAdapters = append(validAdapters, adapter+i)
			}
		}

		diff := validAdapters[0] - adapter
		diffs[diff]++
	}

	result := diffs[1] * diffs[3]

	return strconv.Itoa(result)
}

func (d *Day10) PartTwo(input string) string {
	// Considering 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19
	// Basically count the total for the arrows leaving the number (paths).
	// 19(8) -> 16(8) -> 15(8) -> 12(8)
	//                            |
	//                            +-> 11(4)
	//                            |   |
	//                            |   v
	//                            +-> 10(4) -> 7(4)
	//                                         |
	//                                         +-> 6(2) -+
	//                                         |   |     |
	//                                         |   v     |
	//                                         +-> 5(1)  |
	//                                         |   |     |
	//                                         |   v     |
	//                                         +-> 4(1) <+
	//                                             |
	//                                             +-> 1 (1)
	//
	// 1: _         (1 -> 1 from 0 by default)
	// 4: 1         (1 -> 1 from the 1 + 0 from the 3 + 0 from the 2)
	// 5: 4         (1 -> 1 from the 4 + 0 from the 3 + 0 from the 2)
	// 6: 5, 4      (2 -> 1 from the 5 + 1 from the 4 + 0 from the 3)
	// 7: 6, 5, 4   (4 -> 2 from the 6 + 1 from the 5 + 1 from the 4)
	// 10: 7        (4 -> 0 from the 9 + 0 from the 8 + 4 from the 7)
	// 11: 10       (4 -> 4 from the 10 + 0 from the 9 + 0 from the 8)
	// 12: 11, 10   (8 -> 4 from the 11 + 4 from the 10 + 0 from the 9)
	// 15: 12       (8 -> 0 from the 14 + 0 from the 13 + 8 from the 12)
	// 16: 15       (8 -> 8 from the 15 + 0 from the 14 + 0 from the 13)
	// 19: 16       (8 -> 0 from the 18 + 0 from the 17 + 8 from the 16)

	adapters := readIntSliceInput(input)
	sort.Ints(adapters)
	largestJolts := adapters[len(adapters)-1]

	counter := map[int]int{}
	counter[0] = 1
	for _, item := range adapters {
		counter[item] = 0
		for i := 1; i <= 3; i++ {
			if val, ok := counter[item-i]; ok {
				counter[item] += val
			}
		}
	}

	return strconv.Itoa(counter[largestJolts])
}

func readIntSliceInput(input string) []int {
	lines := strings.Split(input, "\n")
	iInput := make([]int, len(lines))

	for i, item := range lines {
		iInput[i] = utils.Atoi(item)
	}

	return iInput
}
