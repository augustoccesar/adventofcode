package day12

import (
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type Day12 struct{}

func (d *Day12) InputFileName() string { return "input" }

func (d *Day12) PartOne(input string) string {
	commands := strings.Split(input, "\n")
	tracker := map[string]int{"NS": 0, "WE": 0}
	facingIdx := 0 // To be used to query direction

	for _, command := range commands {
		cmd, value := parseCommand(command)

		if cmd == "F" {
			facing := directions[facingIdx]
			moveInDirection(tracker, facing, value)
		}

		if cmd == "R" || cmd == "L" {
			if cmd == "L" {
				value = 360 - value // Equivalent of the left rotation to the right
			}

			facingIdx = (facingIdx + (value / 90)) % 4
		}

		if utils.SliceContains(directions, cmd) {
			moveInDirection(tracker, cmd, value)
		}

	}

	ns := int(math.Abs(float64(tracker["NS"])))
	we := int(math.Abs(float64(tracker["WE"])))

	return strconv.Itoa(ns + we)
}

func (d *Day12) PartTwo(input string) string {
	commands := strings.Split(input, "\n")
	waypoint := map[string]int{"NS": 1, "WE": 10}
	ship := map[string]int{"NS": 0, "WE": 0}

	for _, command := range commands {
		cmd, value := parseCommand(command)

		if cmd == "F" {
			ship["NS"] += waypoint["NS"] * value
			ship["WE"] += waypoint["WE"] * value
		}

		if cmd == "R" || cmd == "L" {
			fValue := utils.DegreeToRadians(value)
			if cmd == "R" {
				fValue = -fValue
			}

			sin := int(math.Sin(fValue))
			cos := int(math.Cos(fValue))

			x := waypoint["WE"]
			y := waypoint["NS"]

			waypoint["WE"] = x*cos - y*sin
			waypoint["NS"] = x*sin + y*cos
		}

		if utils.SliceContains(directions, cmd) {
			moveInDirection(waypoint, cmd, value)
		}
	}

	ns := int(math.Abs(float64(ship["NS"])))
	we := int(math.Abs(float64(ship["WE"])))

	return strconv.Itoa(ns + we)
}

var commandPattern = regexp.MustCompile(`(\w)(\d+)`)
var directions = []string{"E", "S", "W", "N"}

// command, value
func parseCommand(rawCommand string) (string, int) {
	match := commandPattern.FindAllStringSubmatch(rawCommand, -1)[0]
	cmd := match[1]
	value := utils.Atoi(match[2])

	return cmd, value
}

func moveInDirection(movementTracker map[string]int, direction string, amount int) {
	switch direction {
	case "E":
		movementTracker["WE"] += amount
		break
	case "W":
		movementTracker["WE"] -= amount
		break
	case "N":
		movementTracker["NS"] += amount
		break
	case "S":
		movementTracker["NS"] -= amount
		break
	}
}
