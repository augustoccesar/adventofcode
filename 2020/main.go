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

	var taskRunner *utils.TaskRunner
	switch dayInt {
	case 1:
		taskRunner = utils.NewTaskRunner(&day01.Day01{})
	case 2:
		taskRunner = utils.NewTaskRunner(&day02.Day02{})
	case 3:
		taskRunner = utils.NewTaskRunner(&day03.Day03{})
	case 4:
		taskRunner = utils.NewTaskRunner(&day04.Day04{})
	case 5:
		taskRunner = utils.NewTaskRunner(&day05.Day05{})
	case 6:
		taskRunner = utils.NewTaskRunner(&day06.Day06{})
	case 7:
		taskRunner = utils.NewTaskRunner(&day07.Day07{})
	case 8:
		taskRunner = utils.NewTaskRunner(&day08.Day08{})
	case 9:
		taskRunner = utils.NewTaskRunner(&day09.Day09{})
	case 10:
		taskRunner = utils.NewTaskRunner(&day10.Day10{})
	}

	if taskRunner == nil {
		panic("Day not found")
	}

	taskRunner.Run()
}
