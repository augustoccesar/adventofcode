package d09

import (
	"sort"
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
)

type Day09 struct{}

func (d *Day09) Year() int { return 2020 }
func (d *Day09) Day() int  { return 9 }

func (d *Day09) PartOne() string {
	lines := strings.Split(structure.ReadDefaultInput(d), "\n")
	items := make([]int, len(lines))

	for i, item := range lines {
		items[i], _ = strconv.Atoi(item)
	}

	invalid := findInvalid(25, items)

	return strconv.Itoa(invalid)
}

func (d *Day09) PartTwo() string {
	lines := strings.Split(structure.ReadDefaultInput(d), "\n")
	items := make([]int, len(lines))

	for i, item := range lines {
		items[i], _ = strconv.Atoi(item)
	}

	invalid := findInvalid(25, items)
	i, j := findSumRange(invalid, items)

	sumRange := items[i:j]
	sort.Ints(sumRange)
	result := sumRange[0] + sumRange[len(sumRange)-1]

	return strconv.Itoa(result)
}

func containBySum(contained int, possible []int) bool {
	for _, item := range possible {
		counterRequired := contained - item
		for _, item2 := range possible {
			if item2 == counterRequired {
				return true
			}
		}
	}

	return false
}

func findInvalid(lookupSize int, items []int) int {
	invalid := -1
	for i, item := range items {
		if i < lookupSize {
			continue
		}

		valid := containBySum(item, items[i-lookupSize:i])
		if !valid {
			invalid = item
			break
		}
	}

	return invalid
}

func findSumRange(lookup int, items []int) (int, int) {
	for i := 0; i < len(items); i++ {
		sum := items[i]
		for j := i + 1; j < len(items); j++ {
			sum += items[j]

			if sum > lookup {
				break
			}

			if sum == lookup {
				return i, j + 1
			}
		}
	}

	return -1, -1
}
