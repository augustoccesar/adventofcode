package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
}

func partOne() {
	in := readInput()

	total := 0
	groups := strings.Split(in, "\n\n")
	for _, g := range groups {
		groupSet := map[rune]bool{}
		group := strings.Replace(g, "\n", "", -1)
		for _, item := range group {
			groupSet[item] = true
		}

		total += len(groupSet)
	}

	fmt.Printf("Part One: %d\n", total)
}

func partTwo() {
	in := readInput()

	total := 0
	groups := strings.Split(in, "\n\n")
	for _, g := range groups {
		nInGroup := len(strings.Split(g, "\n")) // People in the group
		totalInGroup := 0                       // Items everyone in the group answered "yes"

		groupSet := map[rune]int{}
		group := strings.Replace(g, "\n", "", -1)
		for _, item := range group {
			if val, ok := groupSet[item]; ok {
				groupSet[item] = val + 1
				continue
			}

			groupSet[item] = 1
		}

		for _, count := range groupSet {
			if count == nInGroup { // N of yes to an item == N of people
				totalInGroup++ // Add one item to the total in the group
			}
		}

		total += totalInGroup
	}

	fmt.Printf("Part Two: %d\n", total)
}

func main() {
	partOne()
	partTwo()
}
