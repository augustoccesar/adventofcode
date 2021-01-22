package day19

import (
	"regexp"
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type Day19 struct{}

func (d *Day19) InputFileName() string { return "input" }

func (d *Day19) PartOne(input string) string {
	rules, messages := parseInput(input)

	res := execute(rules, messages, map[int]*visitLimiter{})

	return strconv.Itoa(res)
}

func (d *Day19) PartTwo(input string) string {
	newRules := map[int]loopedRule{
		8:  {"(42 | 42 8)", "(42)"},
		11: {"(42 31 | 42 11 31)", "(42 31)"},
	}
	visitLimiters := map[int]*visitLimiter{
		8:  {currentVisits: 0, maxVisits: 1, replaceAfterMax: newRules[8].fallback},
		11: {currentVisits: 0, maxVisits: 1, replaceAfterMax: newRules[11].fallback},
	}

	lastResult := 0
	sequentialResult := 0
	// If the same result happens more than 2 times on a row, consider it the answer
	for sequentialResult <= 2 {
		rules, messages := parseInput(input)
		rules[8] = newRules[8].rule
		rules[11] = newRules[11].rule

		res := execute(rules, messages, visitLimiters)

		if res == lastResult {
			sequentialResult++
		} else {
			lastResult = res
			sequentialResult = 0
		}

		// Reset limiters, but increase the amount if visits can be made
		for _, limiter := range visitLimiters {
			limiter.currentVisits = 0
			limiter.maxVisits++
		}
	}

	return strconv.Itoa(lastResult)
}

var numberPattern = regexp.MustCompile(`\d+`)

type loopedRule struct {
	rule     string
	fallback string
}

type visitLimiter struct {
	currentVisits   int
	maxVisits       int
	replaceAfterMax string
}

func execute(rules map[int]string, messages []string, visitLimiters map[int]*visitLimiter) int {
	for {
		stillHasNumbers := false

		for i := range rules {
			if numberPattern.MatchString(rules[i]) {
				stillHasNumbers = true

				rules[i] = numberPattern.ReplaceAllStringFunc(rules[i], func(s string) string {
					digit := utils.Atoi(s)

					if limiter, ok := visitLimiters[digit]; ok {
						if limiter.currentVisits > limiter.maxVisits {
							return limiter.replaceAfterMax
						}
						limiter.currentVisits++
					}

					return rules[utils.Atoi(s)]
				})
			}
		}

		if !stillHasNumbers {
			break
		}
	}

	regexRules := map[int]*regexp.Regexp{}
	for i := range rules {
		regexRules[i] = regexp.MustCompile("^" + cleanRule(rules[i]) + "$")
	}

	matches := 0
	for _, message := range messages {
		if regexRules[0].MatchString(message) {
			matches++
		}
	}

	return matches
}

func cleanRule(rule string) string {
	rule = strings.ReplaceAll(rule, "\"", "")
	rule = strings.ReplaceAll(rule, " ", "")

	return rule
}

func parseInput(input string) (rules map[int]string, messages []string) {
	rules = map[int]string{}
	parts := strings.Split(input, "\n\n")

	for _, rule := range strings.Split(parts[0], "\n") {
		ruleParts := strings.Split(rule, ": ")

		ruleKey := utils.Atoi(ruleParts[0])
		ruleValue := "(" + ruleParts[1] + ")" // Group the values into parenthesis for the matching regex

		rules[ruleKey] = ruleValue
	}
	messages = strings.Split(parts[1], "\n")

	return rules, messages
}
