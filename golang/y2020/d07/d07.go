package d07

import (
	"regexp"
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
)

type Day07 struct{}

func (d *Day07) Year() int { return 2020 }
func (d *Day07) Day() int  { return 7 }

func (d *Day07) PartOne() string {
	bagMap := generateBagMap(structure.ReadDefaultInput(d))

	sum := 0
	for _, bag := range bagMap {
		if bag.canFitBag(BagKeyShinyGold, bagMap) {
			sum++
		}
	}

	return strconv.Itoa(sum)
}

func (d *Day07) PartTwo() string {
	bagMap := generateBagMap(structure.ReadDefaultInput(d))

	shinyGoldBag := bagMap[BagKeyShinyGold]
	return strconv.Itoa(shinyGoldBag.totalCapacity(true, bagMap))
}

func generateBagMap(input string) map[string]Bag {
	bagMap := map[string]Bag{}

	rulePattern := regexp.MustCompile(`(\w+\s\w+)\sbags\scontain\s((?:(?:,\s)?\d\s\w+\s\w+\sbag(?:s)?)+|no other bags)`)
	contentPattern := regexp.MustCompile(`(\d)\s(\w+\s\w+)\sbag(?:s)?`)

	ruleMatches := rulePattern.FindAllStringSubmatch(input, -1)

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

const BagKeyShinyGold string = "shiny gold"

type Bag struct {
	Key      string
	Capacity map[string]int
}

func (b *Bag) canFitBag(bagKey string, bagMap map[string]Bag) bool {
	if _, ok := b.Capacity[bagKey]; ok {
		return true
	}

	if len(b.Capacity) == 0 {
		return false
	}

	for nestedBagKey := range b.Capacity {
		nestedBag := bagMap[nestedBagKey]
		if nestedBag.canFitBag(bagKey, bagMap) {
			return true
		}
	}

	return false
}

func (b *Bag) totalCapacity(root bool, bagMap map[string]Bag) int {
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

		total += v * nestedBag.totalCapacity(false, bagMap)
	}

	return total
}
