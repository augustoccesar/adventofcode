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
	bagMap := map[string]Bag{}

	rulePattern := regexp.MustCompile(`(\w+\s\w+)\sbags\scontain\s((?:(?:,\s)?\d\s\w+\s\w+\sbag(?:s)?)+|no other bags)`)
	contentPattern := regexp.MustCompile(`(\d)\s(\w+\s\w+)\sbag(?:s)?`)

	ruleMatches := rulePattern.FindAllStringSubmatch(in, -1)

	for _, ruleMatch := range ruleMatches {
		key := ruleMatch[1]
		contents := ruleMatch[2]

		bag := Bag{Key: key, Capacity: map[string]int{}}

		if contents == "no other bags" {
			continue
		}

		for _, item := range strings.Split(contents, ", ") {
			contentMatches := contentPattern.FindAllStringSubmatch(item, -1)
			for _, contentMatch := range contentMatches {
				amount, _ := strconv.Atoi(contentMatch[1])

				bag.Capacity[contentMatch[2]] = amount
			}
		}

		bagMap[bag.Key] = bag
	}

	return bagMap
}

// Global Bags map to be used for the solutions
var bagMap map[string]Bag = generateBagMap()

const BagKeyShinyGold string = "shiny gold"

type Bag struct {
	Key      string
	Capacity map[string]int
}

func (b *Bag) canFitBag(bagKey string) bool {
	if _, ok := b.Capacity[bagKey]; ok {
		return true
	}

	if len(b.Capacity) == 0 {
		return false
	}

	for nestedBagKey := range b.Capacity {
		nestedBag := bagMap[nestedBagKey]
		if nestedBag.canFitBag(bagKey) {
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
		if bag.canFitBag(BagKeyShinyGold) {
			sum++
		}
	}

	fmt.Printf("Part One: %d\n", sum)
}

func partTwo() {
	shinyGoldBag := bagMap[BagKeyShinyGold]
	fmt.Printf("Part Two: %d\n", shinyGoldBag.totalCapacity(true))
}

func main() {
	partOne()
	partTwo()
}
