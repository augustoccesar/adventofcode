package main

import (
	"context"
	"fmt"
	"maps"
	"os"
	"slices"
	"sort"
	"strconv"
	"strings"

	"github.com/urfave/cli/v3"

	"com.github/augustoccesar/adventofcode/golang/structure"
	"com.github/augustoccesar/adventofcode/golang/y2020"
	// CODEGEN:import_year_package
)

var yearsMap = map[int]map[int]structure.Day{
	2020: y2020.DaysMap,
	// CODEGEN:register_year
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

					daysMap, yearFound := yearsMap[yearInput]
					if !yearFound {
						return cli.Exit(fmt.Sprintf("Day %d for year %d not found", dayInput, yearInput), 1)
					}

					day, dayFound := daysMap[dayInput]
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
			{
				Name:  "days",
				Usage: "list all the years and the included days",
				Action: func(ctx context.Context, cmd *cli.Command) error {
					for year := range yearsMap {
						keys := maps.Keys(yearsMap[year])
						dayKeys := slices.Collect(keys)
						sort.Ints(dayKeys)

						dayStrings := make([]string, len(dayKeys))
						for i, day := range dayKeys {
							dayStrings[i] = strconv.Itoa(day)
						}

						fmt.Printf("%d;%s\n", year, strings.Join(dayStrings, ";"))
					}

					return nil
				},
			},
		},
	}

	cmd.Run(context.Background(), os.Args)
}
