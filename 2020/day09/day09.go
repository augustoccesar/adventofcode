// TODO: This one is looking quite unoptimized. Need a revisit.
package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
)

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
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

func partOne() {
	input := strings.Split(readInput(), "\n")
	items := make([]int, len(input))

	for i, item := range input {
		items[i], _ = strconv.Atoi(item)
	}

	invalid := findInvalid(25, items)

	fmt.Printf("Part One: %d\n", invalid)
}

func partTwo() {
	input := strings.Split(readInput(), "\n")
	items := make([]int, len(input))

	for i, item := range input {
		items[i], _ = strconv.Atoi(item)
	}

	invalid := findInvalid(25, items)
	i, j := findSumRange(invalid, items)

	sumRange := items[i:j]
	sort.Ints(sumRange)
	result := sumRange[0] + sumRange[len(sumRange)-1]

	fmt.Printf("Part Two: %d\n", result)
}

func main() {
	partOne()
	partTwo()
}
