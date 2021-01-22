package utils

import (
	"fmt"
	"strings"
)

type Task interface {
	InputFileName() string
	PartOne(input string) string
	PartTwo(input string) string
}

type TaskRunner struct {
	task Task
}

func NewTaskRunner(task Task) *TaskRunner {
	return &TaskRunner{
		task: task,
	}
}

func (tr *TaskRunner) Run() {
	inputPath := fmt.Sprintf("./inputs/%s_%s.txt", strings.ToLower(GetType(tr.task)), tr.task.InputFileName())
	input := ReadFile(inputPath)

	fmt.Printf("Part One: %s\n", tr.task.PartOne(input))
	fmt.Printf("Part Two: %s\n", tr.task.PartTwo(input))
}
