package d25

import (
	"strconv"
	"strings"

	"com.github/augustoccesar/adventofcode/golang/structure"
	"com.github/augustoccesar/adventofcode/golang/y2020/utils"
)

type Day25 struct{}

func (d *Day25) Year() int { return 2020 }
func (d *Day25) Day() int  { return 25 }

func (d *Day25) PartOne() string {
	lines := strings.Split(structure.ReadDefaultInput(d), "\n")
	cardPublicKey := utils.Atoi(lines[0])
	doorPublicKey := utils.Atoi(lines[1])

	cardLoopSize := getLoopSize(cardPublicKey)
	encryption := getEncryption(doorPublicKey, cardLoopSize)

	return strconv.Itoa(encryption)
}

func (d *Day25) PartTwo() string {
	return "N/A"
}

func getLoopSize(publicKey int) int {
	loopSize := 0
	for key := 1; key != publicKey; loopSize++ {
		key = transform(key, 7)
	}
	return loopSize
}

func getEncryption(publicKey, loopSize int) int {
	encryption := 1
	for l := 0; l < loopSize; l++ {
		encryption = transform(encryption, publicKey)
	}

	return encryption
}

func transform(value, subjectNumber int) int {
	return (value * subjectNumber) % 20201227
}
