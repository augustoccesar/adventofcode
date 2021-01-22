package day18

import (
	"regexp"
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type Day18 struct{}

func (d *Day18) InputFileName() string { return "input" }

func (d *Day18) PartOne(input string) string {
	expressions := strings.Split(input, "\n")

	sum := 0
	for _, exp := range expressions {
		sum += calculate(exp, false)
	}

	return strconv.Itoa(sum)
}

func (d *Day18) PartTwo(input string) string {
	expressions := strings.Split(input, "\n")

	sum := 0
	for _, exp := range expressions {
		sum += calculate(exp, true)
	}

	return strconv.Itoa(sum)
}

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

		str = utils.StringReplace(str, strconv.Itoa(result), start, end-1)
	}

	// If is inverse, calculate the sums first
	if inverse {
		for sumPattern.MatchString(str) {
			matchIdxs := sumPattern.FindAllStringIndex(str, -1)
			start := matchIdxs[0][0]
			end := matchIdxs[0][1]

			expression := str[start:end]
			result := calculate(expression, false)

			str = utils.StringReplace(str, strconv.Itoa(result), start, end-1)
		}
	}

	items := strings.Fields(str)
	calcResult := utils.Atoi(items[0])

	for i := 1; i < len(items); i += 2 {
		op := items[i]
		nextItem := utils.Atoi(items[i+1])

		if op == "+" {
			calcResult += nextItem
		}

		if op == "*" {
			calcResult *= nextItem
		}
	}

	return calcResult
}
