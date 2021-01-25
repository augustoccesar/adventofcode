package main

import (
	"encoding/csv"
	"io"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
	"testing"

	"github.com/augustoccesar/adventofcode/utils"
)

type FailureType string

const (
	FailureTypeNotFound   FailureType = "Not found"
	FailureTypeWrongValue FailureType = "Wrong value"
)

type Failure struct {
	day   int
	part  int
	fType FailureType
}

func TestMain(t *testing.T) {
	str, err := ioutil.ReadFile("expected_results")
	if err != nil {
		t.Fatal("Failed to load the expected results file")
	}

	taskMap := getAvailableDays()
	reader := csv.NewReader(strings.NewReader(string(str)))
	reader.Comma = ';'

	failures := []Failure{}

	for {
		record, err := reader.Read()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatal(err)
		}

		dayInt, err := strconv.Atoi(record[0])
		if err != nil {
			log.Fatal(err)
		}

		task, found := taskMap[dayInt]
		if !found {
			failures = append(
				failures,
				Failure{day: dayInt, part: 1, fType: FailureTypeNotFound},
				Failure{day: dayInt, part: 2, fType: FailureTypeNotFound},
			)
			continue
		}

		runner := utils.NewTaskRunner(task)
		resPartOne, resPartTwo := runner.RunReturn()

		if resPartOne != record[1] {
			failures = append(
				failures,
				Failure{day: dayInt, part: 1, fType: FailureTypeWrongValue},
			)
		}

		if resPartTwo != record[2] {
			failures = append(
				failures,
				Failure{day: dayInt, part: 2, fType: FailureTypeWrongValue},
			)
		}
	}

	for _, failure := range failures {
		t.Logf("Day %d: Failed Part %d. Reason: %s", failure.day, failure.part, failure.fType)
	}

	if len(failures) > 0 {
		t.Fail()
	}
}
