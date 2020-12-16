// Kinda messy. Will take some time to go over it again at other time
package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

type Range struct {
	Start int
	End   int
}

type Rule struct {
	Name   string
	Ranges []Range
}

func (r *Rule) IsValid(val int) bool {
	for _, ran := range r.Ranges {
		if val >= ran.Start && val <= ran.End {
			return true
		}
	}

	return false
}

func (r *Rule) IsAllValid(items []int) bool {
	for _, val := range items {
		if !r.IsValid(val) {
			return false
		}
	}

	return true
}

var rulePattern = regexp.MustCompile(`(.+):\s(\d+)-(\d+)\sor\s(\d+)-(\d+)`)

func generateRule(line string) Rule {
	match := rulePattern.FindAllStringSubmatch(line, -1)[0]
	return Rule{
		Name: match[1],
		Ranges: []Range{
			{Start: atoi(match[2]), End: atoi(match[3])},
			{Start: atoi(match[4]), End: atoi(match[5])},
		},
	}
}

func getValidTickets(rules []Rule, tickets [][]int) ([][]int, []int) {
	validTickets := [][]int{}
	invalidValues := []int{}

	for _, ticket := range tickets {
		validTicket := true
		for _, val := range ticket {
			validVal := false
			for _, rule := range rules {
				validVal = rule.IsValid(val)
				if validVal {
					break
				}
			}

			if !validVal {
				validTicket = false
				invalidValues = append(invalidValues, val)
			}
		}

		if validTicket {
			validTickets = append(validTickets, ticket)
		}
	}

	return validTickets, invalidValues
}

func partOne() {
	rules, _, nearbyTickets := parseInput()
	_, invalidValues := getValidTickets(rules, nearbyTickets)

	sum := 0
	for _, val := range invalidValues {
		sum += val
	}

	fmt.Printf("Part One: %d\n", sum)
}

func partTwo() {
	rules, myTicket, nearbyTickets := parseInput()
	validTickets, _ := getValidTickets(rules, nearbyTickets)

	groups := make([][]int, len(rules))
	for _, ticket := range validTickets {
		for j, val := range ticket {
			groups[j] = append(groups[j], val)
		}
	}

	m := make([][]string, len(groups))
	for i, v := range groups {
		for _, rule := range rules {
			if rule.IsAllValid(v) {
				m[i] = append(m[i], rule.Name)
			}
		}
	}

	departure := []int{}
	res := map[string]int{}
	for len(res) < len(rules) {
		for i, v := range m {
			if len(v) == 1 {
				res[v[0]] = i
				if strings.HasPrefix(v[0], "departure") {
					departure = append(departure, i)
				}
				removeFromAll(v[0], m)
			}
		}
	}

	mult := 1
	for _, val := range departure {
		mult *= myTicket[val]
	}

	fmt.Printf("Part Two: %d\n", mult)
}

// --------------------------------------------------------------------------------------------------------------------

func parseInput() ([]Rule, []int, [][]int) {
	inputParts := strings.Split(readInput(), "\n\n")
	ruleLines := strings.Split(inputParts[0], "\n")
	myTicketLine := strings.Replace(inputParts[1], "your ticket:\n", "", -1)
	nearbyTicketsLines := strings.Split(strings.Replace(inputParts[2], "nearby tickets:\n", "", -1), "\n")

	rules := make([]Rule, len(ruleLines))
	myTicket := make([]int, len(rules))
	nearbyTickets := make([][]int, len(nearbyTicketsLines))

	for i, ruleLine := range ruleLines {
		rules[i] = generateRule(ruleLine)
	}

	for i, item := range strings.Split(myTicketLine, ",") {
		myTicket[i] = atoi(item)
	}

	for i, line := range nearbyTicketsLines {
		nearbyTickets[i] = make([]int, len(rules))
		for j, item := range strings.Split(line, ",") {
			nearbyTickets[i][j] = atoi(item)
		}
	}

	return rules, myTicket, nearbyTickets
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

func remove(str string, arr []string) []string {
	idx := -1
	for i, item := range arr {
		if item == str {
			idx = i
		}
	}

	if idx == -1 {
		return arr
	}

	arr[idx] = arr[len(arr)-1]
	arr[len(arr)-1] = ""
	arr = arr[:len(arr)-1]

	return arr
}

func removeFromAll(str string, arr [][]string) [][]string {
	for i, dArr := range arr {
		arr[i] = remove(str, dArr)
	}

	return arr
}
