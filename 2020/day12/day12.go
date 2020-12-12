package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"regexp"
	"strconv"
	"strings"
)

func partOne() {
	commandPattern := regexp.MustCompile(`(\w)(\d+)`)
	commands := strings.Split(readInput(), "\n")

	directions := []string{"E", "S", "W", "N"}
	facingIdx := 0

	sum := map[string]int{"NS": 0, "WE": 0}

	for _, command := range commands {
		match := commandPattern.FindAllStringSubmatch(command, -1)[0]
		cmd := match[1]
		value, _ := strconv.Atoi(match[2])

		if cmd == "F" {
			facing := directions[facingIdx]

			switch facing {
			case "E":
				sum["WE"] += value
				break
			case "W":
				sum["WE"] -= value
				break
			case "N":
				sum["NS"] += value
				break
			case "S":
				sum["NS"] -= value
				break
			}
		}

		if cmd == "R" || cmd == "L" {
			base := 0
			if cmd == "R" {
				base = 0
			} else {
				base = 360
				value *= -1
			}

			v := (base + value) / 90

			facingIdx = (facingIdx + v) % 4
		}

		if inArr(cmd, directions) {
			switch cmd {
			case "E":
				sum["WE"] += value
				break
			case "W":
				sum["WE"] -= value
				break
			case "N":
				sum["NS"] += value
				break
			case "S":
				sum["NS"] -= value
				break
			}
		}

	}

	ns := int(math.Abs(float64(sum["NS"])))
	we := int(math.Abs(float64(sum["WE"])))
	fmt.Printf("Part One: %d\n", ns+we)
}

func partTwo() {
	commandPattern := regexp.MustCompile(`(\w)(\d+)`)
	commands := strings.Split(readInput(), "\n")

	directions := []string{"E", "S", "W", "N"}
	waypoint := map[string]int{"NS": 1, "WE": 10}
	ship := map[string]int{"NS": 0, "WE": 0}

	for _, command := range commands {
		match := commandPattern.FindAllStringSubmatch(command, -1)[0]
		cmd := match[1]
		value, _ := strconv.Atoi(match[2])

		if cmd == "F" {
			ship["NS"] += waypoint["NS"] * value
			ship["WE"] += waypoint["WE"] * value
		}

		if cmd == "R" || cmd == "L" {
			fValue := degreeToRadians(value)
			if cmd == "R" {
				fValue = degreeToRadians(360) - fValue
			}

			sin := int(math.Sin(fValue))
			cos := int(math.Cos(fValue))

			x := waypoint["WE"]
			y := waypoint["NS"]

			waypoint["WE"] = x*cos - y*sin
			waypoint["NS"] = x*sin + y*cos
		}

		if inArr(cmd, directions) {
			switch cmd {
			case "E":
				waypoint["WE"] += value
				break
			case "W":
				waypoint["WE"] -= value
				break
			case "N":
				waypoint["NS"] += value
				break
			case "S":
				waypoint["NS"] -= value
				break
			}
		}
	}

	ns := int(math.Abs(float64(ship["NS"])))
	we := int(math.Abs(float64(ship["WE"])))

	fmt.Printf("Part Two: %d\n", we+ns)
}

func degreeToRadians(degrees int) float64 {
	return float64(degrees) * (math.Pi / 180.0)
}

func inArr(str string, arr []string) bool {
	for _, item := range arr {
		if item == str {
			return true
		}
	}

	return false
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
}
