package main

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

var rulePattern = regexp.MustCompile(`(.+):\s(\d+)-(\d+)\sor\s(\d+)-(\d+)`)

// Range holds a range start and end value. Both inclusive.
type Range struct {
	Start int
	End   int
}

// IsWithin check if a value is within the range.
func (r *Range) IsWithin(val int) bool {
	if val >= r.Start && val <= r.End {
		return true
	}

	return false
}

// Rule holds a rule name and its ranges.
type Rule struct {
	Name   string
	Ranges []Range
}

// IsValid check if a value is within any of the Rule ranges.
func (r *Rule) IsValid(val int) bool {
	for _, ran := range r.Ranges {
		if ran.IsWithin(val) {
			return true
		}
	}

	return false
}

// IsAllValid check if all items in a slice of values are within any of the Rule ranges.
func (r *Rule) IsAllValid(items []int) bool {
	for _, val := range items {
		if !r.IsValid(val) {
			return false
		}
	}

	return true
}

// generateRule transform an input line into a Rule.
// Example:
//   class: 1-3 or 5-7
// Generates:
//   Rule{Name: class, Ranges: {{Start: 1, End: 3}, {Start: 5, End: 7}}}
func generateRule(line string) Rule {
	match := rulePattern.FindAllStringSubmatch(line, -1)[0]
	return Rule{
		Name: match[1],
		Ranges: []Range{
			{Start: utils.Atoi(match[2]), End: utils.Atoi(match[3])},
			{Start: utils.Atoi(match[4]), End: utils.Atoi(match[5])},
		},
	}
}

// getValidTickets apply the rules to a slice of tickets (which themselves are slices of ints)
// then return a slice of tickets that are valid and a slice with the values that caused tickets
// to be invalid
func getValidTickets(rules []Rule, tickets [][]int) (validTickets [][]int, invalidValues []int) {
	for _, ticket := range tickets { // For each ticket do:
		validTicket := true
		for _, val := range ticket { // For each value inside the ticket do:
			validVal := false
			for _, rule := range rules { // Check the value against each rule:
				validVal = rule.IsValid(val)
				// Since it only need match one rule, as soon as it is valid for one rule, break
				if validVal {
					break
				}
			}

			// If any value is invalid, the whole ticket is invalid.
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

	// Group the ticket values by index
	// For example, for tickets:
	//   - 5, 6, 7
	//   - 10, 15, 16
	// Will generate groups:
	//   [0] -> 5, 10
	//   [1] -> 6, 15
	//   [2] -> 7, 16
	groups := make([][]int, len(rules))
	for _, ticket := range validTickets {
		for j, val := range ticket {
			groups[j] = append(groups[j], val)
		}
	}

	// Aggregate the valid fields per group defined above
	groupsValidFields := make([][]string, len(groups))
	for i, v := range groups {
		for _, rule := range rules {
			// If for all values in group the rule is valid, add the rule as valid field for group
			if rule.IsAllValid(v) {
				groupsValidFields[i] = append(groupsValidFields[i], rule.Name)
			}
		}
	}

	departure := []int{} // To store index of fields that start with "departure"
	setAmount := 0       // Amount of fields that are assigned to group index
	for setAmount < len(groups) {
		for groupIdx, validFields := range groupsValidFields {
			if len(validFields) == 1 {
				if strings.HasPrefix(validFields[0], "departure") {
					departure = append(departure, groupIdx)
				}

				// Since the field is now assigned to a group index, remove it from all the other
				// groups
				utils.MatrixRemove(groupsValidFields, validFields[0])

				setAmount++
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

func parseInput() (rules []Rule, myTicket []int, nearbyTickets [][]int) {
	inputParts := strings.Split(utils.ReadFile("./input.txt"), "\n\n")
	ruleLines := strings.Split(inputParts[0], "\n")
	myTicketLine := strings.Replace(inputParts[1], "your ticket:\n", "", -1)
	nearbyTicketsLines := strings.Split(strings.Replace(inputParts[2], "nearby tickets:\n", "", -1), "\n")

	rules = make([]Rule, len(ruleLines))
	myTicket = make([]int, len(rules))
	nearbyTickets = make([][]int, len(nearbyTicketsLines))

	for i, ruleLine := range ruleLines {
		rules[i] = generateRule(ruleLine)
	}

	for i, item := range strings.Split(myTicketLine, ",") {
		myTicket[i] = utils.Atoi(item)
	}

	for i, line := range nearbyTicketsLines {
		nearbyTickets[i] = make([]int, len(rules))
		for j, item := range strings.Split(line, ",") {
			nearbyTickets[i][j] = utils.Atoi(item)
		}
	}

	return rules, myTicket, nearbyTickets
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
