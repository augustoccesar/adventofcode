package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"

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

	if os.Args[1] == "check" {
		exportResults()
		os.Exit(0)
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
	switch day {
	case 1:
		return utils.NewTaskRunner(&day01.Day01{})
	case 2:
		return utils.NewTaskRunner(&day02.Day02{})
	case 3:
		return utils.NewTaskRunner(&day03.Day03{})
	case 4:
		return utils.NewTaskRunner(&day04.Day04{})
	case 5:
		return utils.NewTaskRunner(&day05.Day05{})
	case 6:
		return utils.NewTaskRunner(&day06.Day06{})
	case 7:
		return utils.NewTaskRunner(&day07.Day07{})
	case 8:
		return utils.NewTaskRunner(&day08.Day08{})
	case 9:
		return utils.NewTaskRunner(&day09.Day09{})
	case 10:
		return utils.NewTaskRunner(&day10.Day10{})
	case 11:
		return utils.NewTaskRunner(&day11.Day11{})
	case 12:
		return utils.NewTaskRunner(&day12.Day12{})
	case 13:
		return utils.NewTaskRunner(&day13.Day13{})
	case 14:
		return utils.NewTaskRunner(&day14.Day14{})
	case 15:
		return utils.NewTaskRunner(&day15.Day15{})
	case 16:
		return utils.NewTaskRunner(&day16.Day16{})
	case 17:
		return utils.NewTaskRunner(&day17.Day17{})
	case 18:
		return utils.NewTaskRunner(&day18.Day18{})
	case 19:
		return utils.NewTaskRunner(&day19.Day19{})
	case 20:
		return utils.NewTaskRunner(&day20.Day20{})
	case 21:
		return utils.NewTaskRunner(&day21.Day21{})
	case 22:
		return utils.NewTaskRunner(&day22.Day22{})
	case 23:
		return utils.NewTaskRunner(&day23.Day23{})
	case 24:
		return utils.NewTaskRunner(&day24.Day24{})
	case 25:
		return utils.NewTaskRunner(&day25.Day25{})
	default:
		return nil
	}
}

// {day};{part one};{part two};
func exportResults() {
	daysRange := utils.MakeRange(1, 25)
	result := strings.Builder{}

	for _, val := range daysRange {
		taskRunner := getDayTask(val)

		result.WriteString(taskRunner.Export())
		result.WriteRune('\n')
	}

	err := ioutil.WriteFile("./results", []byte(strings.TrimSuffix(result.String(), "\n")), 0644)
	if err != nil {
		panic(err)
	}
}
