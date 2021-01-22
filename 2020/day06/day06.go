package day06

import (
	"strconv"
	"strings"
)

type Day06 struct{}

func (d *Day06) InputFileName() string { return "input" }

func (d *Day06) PartOne(input string) string {
	total := 0
	groups := strings.Split(input, "\n\n")
	for _, g := range groups {
		groupSet := map[rune]bool{}
		group := strings.Replace(g, "\n", "", -1)
		for _, item := range group {
			groupSet[item] = true
		}

		total += len(groupSet)
	}

	return strconv.Itoa(total)
}

func (d *Day06) PartTwo(input string) string {
	total := 0
	groups := strings.Split(input, "\n\n")
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

	return strconv.Itoa(total)
}
