package main

import (
	"fmt"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

func partOne() {
	input := strings.Split(utils.ReadFile("./input.txt"), "\n")
	cardPublicKey := utils.Atoi(input[0])
	doorPublicKey := utils.Atoi(input[1])

	cardLoopSize := getLoopSize(cardPublicKey)
	encryption := getEncryption(doorPublicKey, cardLoopSize)

	fmt.Printf("Part One: %d\n", encryption)
}

func partTwo() {
	fmt.Printf("Part Two: Nothing 🎉\n")
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

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
