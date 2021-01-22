package day25

import (
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

type Day25 struct{}

func (d *Day25) InputFileName() string { return "input" }

func (d *Day25) PartOne(input string) string {
	lines := strings.Split(input, "\n")
	cardPublicKey := utils.Atoi(lines[0])
	doorPublicKey := utils.Atoi(lines[1])

	cardLoopSize := getLoopSize(cardPublicKey)
	encryption := getEncryption(doorPublicKey, cardLoopSize)

	return strconv.Itoa(encryption)
}

func (d *Day25) PartTwo(input string) string {
	return ""
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
