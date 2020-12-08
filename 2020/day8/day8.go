package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
}

func run(instructions [][]string) (int, bool) {
	acc := 0
	hist := map[int]bool{}

	i := 0
	stop := false

	for !stop {
		if _, ok := hist[i]; ok || i == len(instructions) {
			stop = true
			break
		}
		hist[i] = true

		inst := instructions[i]

		cmd := inst[0]
		value, _ := strconv.Atoi(inst[1])

		if cmd == "nop" {
			i++
			continue
		}

		if cmd == "acc" {
			acc += value
			i++
			continue
		}

		if cmd == "jmp" {
			i += value
			continue
		}
	}

	return acc, i == len(instructions)
}

func deepCopy(in [][]string) [][]string {
	out := make([][]string, len(in))
	for i, v := range in {
		out[i] = make([]string, len(v))
		copy(out[i], v)
	}

	return out
}

func partOne() {
	lines := strings.Split(readInput(), "\n")
	instructions := [][]string{}

	for _, line := range lines {
		instructions = append(instructions, strings.Split(line, " "))
	}

	acc, _ := run(instructions)

	fmt.Printf("Part One: %d\n", acc)
}

func partTwo() {
	lines := strings.Split(readInput(), "\n")
	instructions := [][]string{}

	for _, line := range lines {
		instructions = append(instructions, strings.Split(line, " "))
	}

	acc := 0
	for i, inst := range instructions {
		if inst[0] != "acc" {
			cpyInst := deepCopy(instructions)

			if inst[0] == "nop" {
				cpyInst[i][0] = "jmp"
			} else {
				cpyInst[i][0] = "nop"
			}

			deepAcc, finished := run(cpyInst) // Brute forcing the sh*t out of it
			if finished {
				acc = deepAcc
			}
		}
	}

	fmt.Printf("Part Two: %d\n", acc)
}

func main() {
	partOne()
	partTwo()
}
