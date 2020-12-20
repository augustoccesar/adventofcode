package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

type tile struct {
	id   int
	data [][]string
}

func partOne() {
	parseInput()
}

func partTwo() {
	parseInput()
}

func parseInput() []tile {
	reg := regexp.MustCompile(`Tile\s(\d+):`)

	input := readInput()
	tiles := []tile{}

	for _, tileData := range strings.Split(input, "\n\n") {
		lines := strings.Split(tileData, "\n")

		id := reg.FindStringSubmatch(lines[0])[1]
		t := tile{id: atoi(id), data: make([][]string, len(lines))}
		for rowIdx, row := range lines[1:] {
			cols := strings.Split(row, "")
			t.data[rowIdx] = make([]string, len(cols))
			for colIdx, item := range cols {
				t.data[rowIdx][colIdx] = item
			}
		}

		tiles = append(tiles, t)
	}

	fmt.Printf("%+v\n", tiles)
	return tiles
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}

func readInput() string {
	// input, err := ioutil.ReadFile("./input.txt")
	input, err := ioutil.ReadFile("./example.txt")
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
