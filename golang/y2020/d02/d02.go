package d02

import (
	"regexp"
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
)

var pattern = regexp.MustCompile(`(\d{1,2})-(\d{1,2})\s(\w{1}):\s(\w*)`)

type Day02 struct{}

func (d *Day02) Year() int { return 2020 }
func (d *Day02) Day() int  { return 2 }

func (d *Day02) PartOne() string {
	valid := 0
	rows := strings.Split(structure.ReadDefaultInput(d), "\n")
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

	return strconv.Itoa(valid)
}

func (d *Day02) PartTwo() string {
	valid := 0
	rows := strings.Split(structure.ReadDefaultInput(d), "\n")
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

	return strconv.Itoa(valid)
}
