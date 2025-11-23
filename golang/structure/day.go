package structure

type Day interface {
	Year() int
	Day() int
	PartOne() string
	PartTwo() string
}
