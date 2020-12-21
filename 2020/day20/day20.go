package main

import (
	"fmt"
	"math"
	"regexp"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type side = string

const (
	sideTop    side = "T"
	sideLeft   side = "L"
	sideRight  side = "R"
	sideBottom side = "B"
)

type tile struct {
	id            int
	data          [][]string
	neighborTiles map[side]*tile
}

func (t *tile) rotate() *tile {
	newTile := t.clone()

	newTile.data = utils.MatrixRotate(t.data)

	return newTile
}

func (t *tile) flip() *tile {
	newTile := t.clone()

	newTile.data = utils.MatrixFlip(t.data)

	return newTile
}

func (tileA *tile) fit(tileB *tile) (bool, side, side) {
	if tileA.id == tileB.id {
		return false, "", ""
	}

	alignments := [][]side{
		{sideBottom, sideTop},
		{sideRight, sideLeft},
		{sideTop, sideBottom},
		{sideLeft, sideRight},
	}

	for _, alignment := range alignments {
		if tileA.getDataSide(alignment[0]) == tileB.getDataSide(alignment[1]) {
			return true, alignment[0], alignment[1]
		}
	}

	return false, "", ""
}

func (t *tile) getDataSide(side side) string {
	switch side {
	case sideTop:
		return strings.Join(t.data[0], "")
	case sideRight:
		data := make([]string, len(t.data))
		for i, row := range t.data {
			data[i] = row[len(row)-1]
		}
		return strings.Join(data, "")
	case sideBottom:
		return strings.Join(t.data[len(t.data)-1], "")
	case sideLeft:
		data := make([]string, len(t.data))
		for i, row := range t.data {
			data[i] = row[0]
		}
		return strings.Join(data, "")
	default:
		return ""
	}
}

func (t *tile) col(i int) []string {
	return utils.MatrixCol(t.data, i)
}

func (t *tile) clone() *tile {
	newTile := *t

	newTile.data = make([][]string, len(t.data))
	for rowIdx, row := range t.data {
		newTile.data[rowIdx] = make([]string, len(row))
		copy(newTile.data[rowIdx], row)
	}

	return &newTile
}

func (t *tile) mutations() []*tile {
	return []*tile{
		t,
		t.rotate(),
		t.rotate().rotate(),
		t.rotate().rotate().rotate(),
		t.flip(),
		t.flip().rotate(),
		t.flip().rotate().rotate(),
		t.flip().rotate().rotate().rotate(),
	}
}

func (t *tile) isCorner() bool {
	if t.neighborTiles[sideBottom] != nil && t.neighborTiles[sideRight] != nil &&
		t.neighborTiles[sideTop] == nil && t.neighborTiles[sideLeft] == nil {
		// Top left
		return true
	}

	if t.neighborTiles[sideBottom] != nil && t.neighborTiles[sideLeft] != nil &&
		t.neighborTiles[sideTop] == nil && t.neighborTiles[sideRight] == nil {
		// Top right
		return true
	}

	if t.neighborTiles[sideTop] != nil && t.neighborTiles[sideRight] != nil &&
		t.neighborTiles[sideBottom] == nil && t.neighborTiles[sideLeft] == nil {
		// Bottom left
		return true
	}

	if t.neighborTiles[sideTop] != nil && t.neighborTiles[sideLeft] != nil &&
		t.neighborTiles[sideBottom] == nil && t.neighborTiles[sideRight] == nil {
		// Bottom right
		return true
	}

	return false
}

func (t *tile) removeBorders() *tile {
	newTile := t.clone()

	newData := make([][]string, len(t.data)-2)
	for i, di := 1, 0; i < len(t.data)-1; i, di = i+1, di+1 {
		newData[di] = make([]string, len(t.data)-2)
		for j, dj := 1, 0; j < len(t.data[i])-1; j, dj = j+1, dj+1 {
			newData[di][dj] = t.data[i][j]
		}
	}

	newTile.data = newData

	return newTile
}

func (t *tile) String() string {
	var repr strings.Builder

	fmt.Fprintf(&repr, "ID: %d\n", t.id)
	for _, row := range t.data {
		for _, item := range row {
			fmt.Fprintf(&repr, "%s ", item)
		}
		fmt.Fprint(&repr, "\n")
	}

	return repr.String()
}

func createNewTile(id int, data [][]string) *tile {
	return &tile{
		id:            id,
		data:          data,
		neighborTiles: map[side]*tile{},
	}
}

func puzzleTogether(tiles []*tile) []*tile {
	assigned := map[int]*tile{}
	unassigned := map[int]*tile{}

	for i, tile := range tiles {
		if i == 0 {
			assigned[tile.id] = tile
			continue
		}

		unassigned[tile.id] = tile

	}

	for len(unassigned) > 0 {
		for _, tileA := range unassigned {
			for _, tileB := range assigned {
				for _, tileAMutation := range tileA.mutations() {
					doesFit, sideB, sideA := tileB.fit(tileAMutation)
					if doesFit {
						tileA = tileAMutation

						assigned[tileA.id] = tileA
						tileB.neighborTiles[sideB] = tileA
						tileA.neighborTiles[sideA] = tileB
						delete(unassigned, tileA.id)
						break
					}
				}
			}
		}
	}

	i := 0
	res := make([]*tile, len(assigned))
	for _, tile := range assigned {
		res[i] = tile
		i++
	}

	return res
}

func combineTiles(tiles []*tile) [][]string {
	var topLeft *tile
	var bottomRight *tile

	for _, tile := range tiles {
		if tile.neighborTiles[sideBottom] != nil && tile.neighborTiles[sideRight] != nil &&
			tile.neighborTiles[sideTop] == nil && tile.neighborTiles[sideLeft] == nil {
			topLeft = tile
		}

		if tile.neighborTiles[sideTop] != nil && tile.neighborTiles[sideLeft] != nil &&
			tile.neighborTiles[sideBottom] == nil && tile.neighborTiles[sideRight] == nil {
			// Bottom right
			bottomRight = tile
		}
	}

	tileCount := len(tiles)
	tileDimension := len(tiles[0].data)
	fullDataDimension := int(math.Sqrt(float64(tileCount))) * tileDimension
	fullData := make([][]string, fullDataDimension)

	fullDataRow := 0

	currTile := topLeft
	rowLeft := topLeft

	reachedEnd := false
	for !reachedEnd {
		for i := 0; i < len(currTile.data); i++ {
			fullData[fullDataRow+i] = append(fullData[fullDataRow+i], currTile.data[i]...)
		}

		if currTile == bottomRight {
			reachedEnd = true
			continue
		}

		if currTile.neighborTiles[sideRight] == nil {
			rowLeft = rowLeft.neighborTiles[sideBottom]
			currTile = rowLeft
			fullDataRow += tileDimension
		} else {
			currTile = currTile.neighborTiles[sideRight]
		}
	}

	return fullData
}

func partOne() {
	tiles := puzzleTogether(parseInput())

	mult := 1
	for _, tile := range tiles {
		if tile.isCorner() {
			mult *= tile.id
		}
	}

	fmt.Printf("Part One: %d\n", mult)
}

func partTwo() {
	tiles := puzzleTogether(parseInput())

	for _, tile := range tiles {
		*tile = *tile.removeBorders()
	}

	combinedTiles := combineTiles(tiles)

	mutations := [][][]string{
		combinedTiles,
		utils.MatrixRotate(combinedTiles),
		utils.MatrixRotate(utils.MatrixRotate(combinedTiles)),
		utils.MatrixRotate(utils.MatrixRotate(utils.MatrixRotate(combinedTiles))),
		utils.MatrixFlip(combinedTiles),
		utils.MatrixRotate(utils.MatrixFlip(combinedTiles)),
		utils.MatrixRotate(utils.MatrixRotate(utils.MatrixFlip(combinedTiles))),
		utils.MatrixRotate(utils.MatrixRotate(utils.MatrixRotate(utils.MatrixFlip(combinedTiles)))),
	}

	amount := 0
	for _, possibleData := range mutations {
		amount = lookForSeamonsters(possibleData)
		if amount > 0 {
			break
		}
	}

	total := utils.MatrixCountItem(combinedTiles, "#")
	occupiedBySeamonsters := amount * 15

	fmt.Printf("Part Two: %d\n", total-occupiedBySeamonsters)
}

//                   #
// #    ##    ##    ###
//  #  #  #  #  #  #
func lookForSeamonsters(data [][]string) int {
	coordinates := func(row, col int) (validStart bool, coordinates [][2]int) {
		coords := [][2]int{
			{row + 1, col - 18},

			{row + 1, col - 13},
			{row + 1, col - 12},

			{row + 1, col - 7},
			{row + 1, col - 6},

			{row + 1, col - 1},
			{row + 1, col},
			{row + 1, col + 1},

			{row + 2, col - 17},
			{row + 2, col - 14},
			{row + 2, col - 11},
			{row + 2, col - 8},
			{row + 2, col - 5},
			{row + 2, col - 2},
		}

		for _, coord := range coords {
			if coord[0] > len(data)-1 || coord[0] < 0 {
				return false, [][2]int{}
			}

			if coord[1] > len(data[0])-1 || coord[1] < 0 {
				return false, [][2]int{}
			}
		}

		return true, coords
	}

	monsters := 0
	for i, row := range data {
		for j, item := range row {
			if item == "#" {
				valid, coords := coordinates(i, j)
				if !valid {
					continue
				}

				data := utils.MatrixLookup(data, coords)
				if utils.SliceAll(data, "#") {
					monsters++
				}
			}
		}
	}

	return monsters
}

// --------------------------------------------------------------------------------------------------------------------

func parseInput() []*tile {
	reg := regexp.MustCompile(`Tile\s(\d+):`)

	input := utils.ReadFile("./input.txt")
	tiles := []*tile{}

	for _, tileData := range strings.Split(input, "\n\n") {
		lines := strings.Split(tileData, "\n")

		id := reg.FindStringSubmatch(lines[0])[1]
		t := createNewTile(utils.Atoi(id), make([][]string, len(lines)-1))
		for rowIdx, row := range lines[1:] {
			cols := strings.Split(row, "")
			t.data[rowIdx] = make([]string, len(cols))
			for colIdx, item := range cols {
				t.data[rowIdx][colIdx] = item
			}
		}

		tiles = append(tiles, t)
	}

	return tiles
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
