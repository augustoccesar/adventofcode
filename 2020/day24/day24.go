package day24

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type Day24 struct{}

func (d *Day24) InputFileName() string { return "input" }

func (d *Day24) PartOne(input string) string {
	blackTiles := parseInput(input)

	return strconv.Itoa(len(blackTiles))
}

func (d *Day24) PartTwo(input string) string {
	blackTiles := parseInput(input)

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

			// Any black tile with zero or more than 2 black tiles immediately adjacent to it
			// is flipped to white. (AKA not black anymore, so check for the opposite range).
			// No need to check for the zero condition since during the creation of blackNeighborsCount
			// it only get ones that have any black neighbor
			if isBlack && blacks <= 2 {
				newBlacks[id] = blackTiles[id]
			} else if !isBlack && blacks == 2 { // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
				newBlacks[id] = NewTileWithCoords(tileIDToCoords(id))
			}
		}

		blackTiles = newBlacks
	}

	return strconv.Itoa(len(blackTiles))
}

func parseInput(input string) map[string]*Tile {
	inputLines := strings.Split(input, "\n")

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

var directionModifiers = map[Direction][]int{
	DirectionNW: {-1, 0, 1},
	DirectionW:  {-1, 1, 0},
	DirectionSW: {0, 1, -1},
	DirectionNE: {0, -1, 1},
	DirectionE:  {1, -1, 0},
	DirectionSE: {1, 0, -1},
}

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
	modifier := directionModifiers[dir]

	t.X = t.X + modifier[0]
	t.Y = t.Y + modifier[1]
	t.Z = t.Z + modifier[2]
}

func (t *Tile) NeighborsIDs() []string {
	ids := make([]string, 6)

	i := 0
	for _, coord := range directionModifiers {
		ids[i] = coordsToID(t.X+coord[0], t.Y+coord[1], t.Z+coord[2])
		i++
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
