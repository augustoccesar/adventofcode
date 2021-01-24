package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/augustoccesar/adventofcode/day01"
	"github.com/augustoccesar/adventofcode/day02"
	"github.com/augustoccesar/adventofcode/day03"
	"github.com/augustoccesar/adventofcode/day04"
	"github.com/augustoccesar/adventofcode/day05"
	"github.com/augustoccesar/adventofcode/day06"
	"github.com/augustoccesar/adventofcode/day07"
	"github.com/augustoccesar/adventofcode/day08"
	"github.com/augustoccesar/adventofcode/day09"
	"github.com/augustoccesar/adventofcode/day10"
	"github.com/augustoccesar/adventofcode/day11"
	"github.com/augustoccesar/adventofcode/day12"
	"github.com/augustoccesar/adventofcode/day13"
	"github.com/augustoccesar/adventofcode/day14"
	"github.com/augustoccesar/adventofcode/day15"
	"github.com/augustoccesar/adventofcode/day16"
	"github.com/augustoccesar/adventofcode/day17"
	"github.com/augustoccesar/adventofcode/day18"
	"github.com/augustoccesar/adventofcode/day19"
	"github.com/augustoccesar/adventofcode/day20"
	"github.com/augustoccesar/adventofcode/day21"
	"github.com/augustoccesar/adventofcode/day22"
	"github.com/augustoccesar/adventofcode/day23"
	"github.com/augustoccesar/adventofcode/day24"
	"github.com/augustoccesar/adventofcode/day25"
	"github.com/augustoccesar/adventofcode/utils"
)

func main() {
	fmt.Printf("%+v\n", os.Args)

	if len(os.Args) < 2 {
		panic("Invalid amount of arguments")
	}

	dayInt, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic("Invalid argument value for day")
	}

	taskRunner := getDayTask(dayInt)

	if taskRunner == nil {
		panic("Day not found")
	}

	taskRunner.Run()
}

func getDayTask(day int) *utils.TaskRunner {
	task, found := getAvailableDays()[day]
	if !found {
		return nil
	}

	return utils.NewTaskRunner(task)
}

func getAvailableDays() map[int]utils.Task {
	return map[int]utils.Task{
		1:  &day01.Day01{},
		2:  &day02.Day02{},
		3:  &day03.Day03{},
		4:  &day04.Day04{},
		5:  &day05.Day05{},
		6:  &day06.Day06{},
		7:  &day07.Day07{},
		8:  &day08.Day08{},
		9:  &day09.Day09{},
		10: &day10.Day10{},
		11: &day11.Day11{},
		12: &day12.Day12{},
		13: &day13.Day13{},
		14: &day14.Day14{},
		15: &day15.Day15{},
		16: &day16.Day16{},
		17: &day17.Day17{},
		18: &day18.Day18{},
		19: &day19.Day19{},
		20: &day20.Day20{},
		21: &day21.Day21{},
		22: &day22.Day22{},
		23: &day23.Day23{},
		24: &day24.Day24{},
		25: &day25.Day25{},
	}
}
