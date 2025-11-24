package main

import (
	"context"
	"fmt"
	"os"

	"github.com/urfave/cli/v3"

	"com.github/augustoccesar/adventofcode/golang/structure"
	"com.github/augustoccesar/adventofcode/golang/y2020"
	// CODEGEN:target_import
)

type DayMapKey struct {
	year int
	day  int
}

var daysMap = map[DayMapKey]structure.Day{
	{2020, 1}: &y2020.Day01{},
	// CODEGEN:target_dict
}

func main() {
	cmd := &cli.Command{
		Commands: []*cli.Command{
			{
				Name:  "run",
				Usage: "run a specific day of a year",
				Arguments: []cli.Argument{
					&cli.IntArg{Name: "year"},
					&cli.IntArg{Name: "day"},
				},
				Action: func(ctx context.Context, cmd *cli.Command) error {
					yearInput := cmd.IntArg("year")
					if yearInput == 0 {
						return cli.Exit("Invalid value for year", 1)
					}

					dayInput := cmd.IntArg("day")
					if dayInput == 0 {
						return cli.Exit("Invalid value for day", 1)
					}

					day, dayFound := daysMap[DayMapKey{yearInput, dayInput}]
					if !dayFound {
						return cli.Exit(fmt.Sprintf("Day %d for year %d not found", dayInput, yearInput), 1)
					}

					partOneResult := day.PartOne()
					fmt.Printf("%s\n", partOneResult)

					partTwoResult := day.PartTwo()
					fmt.Printf("%s\n", partTwoResult)

					return nil
				},
			},
		},
	}

	cmd.Run(context.Background(), os.Args)
}
