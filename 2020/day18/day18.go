package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

var deepestOperationPattern = regexp.MustCompile(`\([^\(\)]+\)`) // Parenthesis that don't contain other parenthesis inside
var sumPattern = regexp.MustCompile(`\d+\s\+\s\d+`)

func calculate(str string, inverse bool) int {
	// Calculate all the deep expressions first
	for deepestOperationPattern.MatchString(str) {
		matchIdxs := deepestOperationPattern.FindAllStringIndex(str, -1)
		start := matchIdxs[0][0]
		end := matchIdxs[0][1]

		expression := str[start+1 : end-1]
		result := calculate(expression, inverse)

		str = replace(str, strconv.Itoa(result), start, end-1)
	}

	// If is inverse, calculate the sums first
	if inverse {
		for sumPattern.MatchString(str) {
			matchIdxs := sumPattern.FindAllStringIndex(str, -1)
			start := matchIdxs[0][0]
			end := matchIdxs[0][1]

			expression := str[start:end]
			result := calculate(expression, false)

			str = replace(str, strconv.Itoa(result), start, end-1)
		}
	}

	items := strings.Fields(str)
	calcResult := atoi(items[0])

	for i := 1; i < len(items); i += 2 {
		op := items[i]
		nextItem := atoi(items[i+1])

		if op == "+" {
			calcResult += nextItem
		}

		if op == "*" {
			calcResult *= nextItem
		}
	}

	return calcResult
}

func partOne() {
	expressions := strings.Split(readInput(), "\n")

	sum := 0
	for _, exp := range expressions {
		sum += calculate(exp, false)
	}

	fmt.Printf("Part One: %d\n", sum)
}

func partTwo() {
	expressions := strings.Split(readInput(), "\n")

	sum := 0
	for _, exp := range expressions {
		sum += calculate(exp, true)
	}

	fmt.Printf("Part Two: %d\n", sum)
}

// There must be a better way of doing this xD
// replace replaces a string with a substring from defined points (both inclusive) on the input string
// Example:
//   input = "a b c d e f"
//   replace = "x"
//   start = 4
//   end = 6
//
//   result = "a b x e f"
func replace(input string, replace string, start, end int) string {
	inputRunes := []rune(input)
	replaceRunes := []rune(replace)

	newSize := len(inputRunes) + len(replace) - ((end - start) + 1)

	result := make([]rune, newSize)

	for i, j := 0, 0; i < len(inputRunes); i, j = i+1, j+1 {
		if i != start {
			result[j] = inputRunes[i]
			continue
		}

		if i == start {
			for l := 0; l < len(replaceRunes); l++ {
				result[i+l] = replaceRunes[l]
			}

			j += len(replaceRunes) - 1
			i = end
		}
	}

	res := string(result)
	return res
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
}

func atoi(str string) int {
	val, err := strconv.Atoi(str)
	if err != nil {
		panic(err)
	}

	return val
}
