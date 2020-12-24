package main

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

func partOne() {
	blackTiles := parseInput()

	fmt.Printf("Part One: %d\n", len(blackTiles))
}

func partTwo() {
	blackTiles := parseInput()

	for i := 0; i < 100; i++ {
		blackNeighborsCount := make(map[string]int)

		for id := range blackTiles {
			for _, nID := range blackTiles[id].NeighborsIDs() {
				blackNeighborsCount[nID]++
			}
		}

		newBlacks := map[string]*Tile{}

		for id, blacks := range blackNeighborsCount {
			_, isBlack := blackTiles[id]
			if isBlack && blacks != 0 && blacks <= 2 { // Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white. (AKA not black anymore)
				newBlacks[id] = blackTiles[id]
			} else if !isBlack && blacks == 2 { // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
				newBlacks[id] = NewTileWithCoords(tileIDToCoords(id))
			}
		}

		blackTiles = newBlacks
	}

	fmt.Printf("Day Two: %d\n", len(blackTiles))
}

func parseInput() map[string]*Tile {
	inputLines := strings.Split(utils.ReadFile("./input.txt"), "\n")

	blackTiles := map[string]*Tile{}
	for _, line := range inputLines {
		tile := NewTile()

		for _, step := range linePattern.FindAllString(line, -1) {
			tile.Move(step)
		}

		if _, ok := blackTiles[tile.ID()]; !ok {
			blackTiles[tile.ID()] = tile
		} else {
			delete(blackTiles, tile.ID())
		}
	}

	return blackTiles
}

// --------------------------------------------------------------------------------------------------------------------

// NW  _  NE
//  W  C  E
// SW  _  SE

var linePattern = regexp.MustCompile(`w|e|se|ne|sw|nw`)

type Direction = string

const (
	DirectionNE Direction = "ne"
	DirectionE  Direction = "e"
	DirectionSE Direction = "se"
	DirectionNW Direction = "nw"
	DirectionW  Direction = "w"
	DirectionSW Direction = "sw"
)

// https://en.wikipedia.org/wiki/Hexagonal_tiling
// https://stackoverflow.com/questions/2459402/hexagonal-grid-coordinates-to-pixel-coordinates
type Tile struct {
	X int
	Y int
	Z int
}

func NewTile() *Tile {
	return &Tile{X: 0, Y: 0, Z: 0}
}

func NewTileWithCoords(x, y, z int) *Tile {
	return &Tile{X: x, Y: y, Z: z}
}

func (t *Tile) ID() string {
	return coordsToID(t.X, t.Y, t.Z)
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

func (t *Tile) NeighborsIDs() []string {
	ids := make([]string, 6)
	coordsFromPoint := [][]int{
		{-1, 0, 1}, // NW
		{-1, 1, 0}, // W
		{0, 1, -1}, // SW
		{0, -1, 1}, // NE
		{1, -1, 0}, // E
		{1, 0, -1}, // SE
	}

	for i, coord := range coordsFromPoint {
		ids[i] = coordsToID(t.X+coord[0], t.Y+coord[1], t.Z+coord[2])
	}

	return ids
}

func coordsToID(x, y, z int) string {
	return fmt.Sprintf("%d,%d,%d", x, y, z)
}

func tileIDToCoords(id string) (x, y, z int) {
	coord := strings.Split(id, ",")
	x, y, z = utils.Atoi(coord[0]), utils.Atoi(coord[1]), utils.Atoi(coord[2])
	return
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
