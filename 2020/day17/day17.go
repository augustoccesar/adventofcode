package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

type status = int

const (
	statusActive   status = 1
	statusInactive status = 0
)

func positionToKey(x, y, z, w int) string {
	return fmt.Sprintf("%d,%d,%d,%d", x, y, z, w)
}

type dimension struct {
	activeCubes map[string]bool
}

func newDimension() dimension {
	return dimension{activeCubes: map[string]bool{}}
}

func (d *dimension) addActiveCube(x, y, z, w int) {
	key := positionToKey(x, y, z, w)

	d.activeCubes[key] = true
}

func (d *dimension) lookupCube(x, y, z, w int) bool {
	return d.activeCubes[positionToKey(x, y, z, w)]
}

func (d *dimension) defineNewStatus(x, y, z, w int) status {
	activeNeighbors := 0
	for px := x - 1; px <= x+1; px++ {
		for py := y - 1; py <= y+1; py++ {
			for pz := z - 1; pz <= z+1; pz++ {
				for pw := w - 1; pw <= w+1; pw++ {
					if pw == w && px == x && py == y && pz == z { // Ignore itself
						continue
					}

					if d.lookupCube(px, py, pz, pw) {
						activeNeighbors++
					}
				}
			}
		}
	}

	currentIsActive := d.lookupCube(x, y, z, w)

	// If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active
	if currentIsActive && (activeNeighbors == 2 || activeNeighbors == 3) {
		return statusActive
	}

	// If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active
	if !currentIsActive && activeNeighbors == 3 {
		return statusActive
	}

	// If doesn't match the above, mean it will be inactive
	return statusInactive
}

func partOne() {
	activeCubes, length, height := parseInput(readInput())
	currentDimension := dimension{activeCubes: activeCubes}

	for i := 1; i <= 6; i++ {
		nextDimension := newDimension()

		// Using the cycle count to expand the dimension on every cycle
		for z := -i; z <= i; z++ {
			for x := -i; x <= length+i; x++ {
				for y := -i; y <= height+i; y++ {
					newStatus := currentDimension.defineNewStatus(x, y, z, 0)
					if newStatus == statusActive {
						nextDimension.addActiveCube(x, y, z, 0)
					}
				}
			}
		}

		currentDimension = nextDimension
	}

	fmt.Printf("Part One: %d\n", len(currentDimension.activeCubes))
}

func partTwo() {
	activeCubes, length, height := parseInput(readInput())
	currentDimension := dimension{activeCubes: activeCubes}

	for i := 1; i <= 6; i++ {
		nextDimension := newDimension()

		for w := -i; w <= i; w++ {
			for z := -i; z <= i; z++ {
				for x := -i; x <= length+i; x++ {
					for y := -i; y <= height+i; y++ {
						newStatus := currentDimension.defineNewStatus(x, y, z, w)
						if newStatus == statusActive {
							nextDimension.addActiveCube(x, y, z, w)
						}
					}
				}
			}
		}

		currentDimension = nextDimension
	}

	fmt.Printf("Part Two: %d\n", len(currentDimension.activeCubes))
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}

func parseInput(input string) (activeCubes map[string]bool, length, height int) {
	activeCubes = map[string]bool{}
	rows := strings.Split(input, "\n")
	length = len(rows[0])
	height = len(rows)

	for y, row := range rows {
		for x, item := range row {
			if string(item) == "#" {
				activeCubes[fmt.Sprintf("%d,%d,%d,%d", x, y, 0, 0)] = true
			}
		}
	}

	return activeCubes, length, height
}

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
}
