package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
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

func generateBagMap() map[string]Bag {
	in := readInput()
	rules := strings.Split(in, "\n")

	bagMap := map[string]Bag{}

	for _, rule := range rules {
		rule = strings.ReplaceAll(rule, "bags", "")
		rule = strings.ReplaceAll(rule, "bag", "")
		rule = strings.ReplaceAll(rule, ".", "")
		rule = strings.ReplaceAll(rule, " , ", "+")
		rule = strings.ReplaceAll(rule, "  ", " ")
		rule = strings.ReplaceAll(rule, " contain ", ":")
		rule = strings.TrimSpace(rule)

		tokens := strings.Split(rule, ":")
		key := tokens[0]
		contents := strings.Split(tokens[1], "+")

		bag := Bag{Key: key, Capacity: map[string]int{}}
		capacityReg := regexp.MustCompile(`(\d)\s(\w+\s\w+)|no other`)
		for _, content := range contents {
			match := capacityReg.FindAllStringSubmatch(content, -1)[0]
			if match[0] == "no other" {
				continue
			}

			val, _ := strconv.Atoi(match[1])
			bag.Capacity[match[2]] = val
		}

		bagMap[bag.Key] = bag
	}

	return bagMap
}

// Global Bags map to be used for the solutions
var bagMap map[string]Bag = generateBagMap()
var shinyGoldKey string = "shiny gold"

type Bag struct {
	Key      string
	Capacity map[string]int
}

func (b *Bag) canFitShinyGold() bool {
	if _, ok := b.Capacity[shinyGoldKey]; ok {
		return true
	}

	if len(b.Capacity) == 0 {
		return false
	}

	for bagKey := range b.Capacity {
		nestedBag := bagMap[bagKey]
		if nestedBag.canFitShinyGold() {
			return true
		}
	}

	return false
}

func (b *Bag) totalCapacity(root bool) int {
	if len(b.Capacity) == 0 {
		return 1
	}

	var total int
	if root {
		// Root bag shouldn't count itself
		// TODO: There is probably a better way to deal with this
		total = 0
	} else {
		total = 1
	}

	for k, v := range b.Capacity {
		nestedBag := bagMap[k]

		total += v * nestedBag.totalCapacity(false)
	}

	return total
}

func partOne() {
	sum := 0
	for _, bag := range bagMap {
		if bag.canFitShinyGold() {
			sum++
		}
	}

	fmt.Printf("Part One: %d\n", sum)
}

func partTwo() {
	shinyGoldBag := bagMap[shinyGoldKey]
	fmt.Printf("Part Two: %d\n", shinyGoldBag.totalCapacity(true))
}

func main() {
	partOne()
	partTwo()
}
