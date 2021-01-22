package day08

import (
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type Day08 struct{}

func (d *Day08) InputFileName() string { return "input" }

func (d *Day08) PartOne(input string) string {
	lines := strings.Split(input, "\n")
	instructions := [][]string{}

	for _, line := range lines {
		instructions = append(instructions, strings.Split(line, " "))
	}

	acc, _ := run(instructions)

	return strconv.Itoa(acc)
}

func (d *Day08) PartTwo(input string) string {
	lines := strings.Split(input, "\n")
	instructions := [][]string{}

	for _, line := range lines {
		instructions = append(instructions, strings.Split(line, " "))
	}

	acc := 0
	for i, inst := range instructions {
		if inst[0] != "acc" {
			cpyInst := utils.MatrixDeepCopy(instructions)

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

	return strconv.Itoa(acc)
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
