package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

var pattern = regexp.MustCompile(`(\d{1,2})-(\d{1,2})\s(\w{1}):\s(\w*)`)

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
}

func partOne() {
	valid := 0
	rows := strings.Split(readInput(), "\n")
	for _, row := range rows {
		match := pattern.FindAllStringSubmatch(row, -1)[0]

		min, err := strconv.Atoi(match[1])
		if err != nil {
			panic(err)
		}
		max, err := strconv.Atoi(match[2])
		if err != nil {
			panic(err)
		}
		char := match[3]
		password := match[4]

		charInstances := strings.Count(password, char)
		if charInstances >= min && charInstances <= max {
			valid++
		}
	}

	fmt.Printf("Part One: %d\n", valid)
}

func partTwo() {
	valid := 0
	rows := strings.Split(readInput(), "\n")
	for _, row := range rows {
		match := pattern.FindAllStringSubmatch(row, -1)[0]

		idx1, err := strconv.Atoi(match[1])
		if err != nil {
			panic(err)
		}
		idx1--

		idx2, err := strconv.Atoi(match[2])
		if err != nil {
			panic(err)
		}
		idx2--

		char := []rune(match[3])[0]
		password := []rune(match[4])

		if (password[idx1] == char && password[idx2] != char) || (password[idx1] != char && password[idx2] == char) {
			valid++
		}

	}

	fmt.Printf("Part Two: %d\n", valid)
}

func main() {
	partOne()
	partTwo()
}
