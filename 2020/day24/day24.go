package main

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

func partOne() {
	inputLines := strings.Split(utils.ReadFile("./input.txt"), "\n")
	// NW  _  NE
	//  W  C  E
	// SW  _  SE

	tilesMap := map[string]*Tile{}
	for _, line := range inputLines {
		tile := NewTile()

		for _, step := range linePattern.FindAllString(line, -1) {
			tile.Move(step)
		}

		if _, ok := tilesMap[tile.ID()]; !ok {
			tilesMap[tile.ID()] = tile
		}

		tilesMap[tile.ID()].Flip()
	}

	colorCount := map[Color]int{ColorWhite: 0, ColorBlack: 0}
	for _, v := range tilesMap {
		colorCount[v.Color]++
	}

	fmt.Printf("Part One: %d\n", colorCount[ColorBlack])
}

func partTwo() {
}

// --------------------------------------------------------------------------------------------------------------------

var linePattern = regexp.MustCompile(`w|e|se|ne|sw|nw`)

type Direction = string
type Color = string

const (
	DirectionNE Direction = "ne"
	DirectionE  Direction = "e"
	DirectionSE Direction = "se"
	DirectionNW Direction = "nw"
	DirectionW  Direction = "w"
	DirectionSW Direction = "sw"

	ColorBlack = "b"
	ColorWhite = "w"
)

type Tile struct {
	X     int
	Y     int
	Z     int
	Color Color
}

func NewTile() *Tile {
	return &Tile{X: 0, Y: 0, Z: 0, Color: ColorWhite}
}

func (t *Tile) ID() string {
	return fmt.Sprintf("%d,%d,%d", t.X, t.Y, t.Z)
}

func (t *Tile) Flip() {
	if t.Color == ColorBlack {
		t.Color = ColorWhite
	} else {
		t.Color = ColorBlack
	}
}

func (t *Tile) Move(dir Direction) {
	switch dir {
	case DirectionNW:
		t.X = t.X - 1
		t.Y = t.Y
		t.Z = t.Z + 1
		break
	case DirectionW:
		t.X = t.X - 1
		t.Y = t.Y + 1
		t.Z = t.Z
		break
	case DirectionSW:
		t.X = t.X
		t.Y = t.Y + 1
		t.Z = t.Z - 1
		break

	case DirectionNE:
		t.X = t.X
		t.Y = t.Y - 1
		t.Z = t.Z + 1
		break
	case DirectionE:
		t.X = t.X + 1
		t.Y = t.Y - 1
		t.Z = t.Z
		break
	case DirectionSE:
		t.X = t.X + 1
		t.Y = t.Y
		t.Z = t.Z - 1
		break
	}
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
