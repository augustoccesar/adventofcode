package main

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/augustoccesar/adventofcode/utils"
)

var memInputPattern = regexp.MustCompile(`mem\[(\d+)\] = (\d+)`)
var maskPattern = regexp.MustCompile(`mask = (.+)`)

func partOne() {
	input := strings.Split(utils.ReadFile("./input.txt"), "\n")

	currentMask := ""
	mem := map[int]int64{}
	for _, item := range input {
		matchMask := matchMask(item)
		if matchMask != "" {
			currentMask = matchMask
			continue
		}

		memAddr, value := matchMemOverride(item)

		valueBinStr := utils.IntToBinStr(value)

		newValueBinStr := applyMask(currentMask, valueBinStr, 1)
		newValueInt := utils.BinStrToInt(newValueBinStr)

		mem[memAddr] = newValueInt
	}

	var sum int64 = 0
	for _, v := range mem {
		sum += v
	}
	fmt.Printf("Part One: %d\n", sum)
}

func partTwo() {
	input := strings.Split(utils.ReadFile("./input.txt"), "\n")

	currentMask := ""
	mem := map[int64]int{}
	for _, item := range input {
		matchMask := matchMask(item)
		if matchMask != "" {
			currentMask = matchMask
			continue
		}

		memAddr, value := matchMemOverride(item)

		addressBin := utils.IntToBinStr(memAddr)
		maskedAddress := applyMask(currentMask, addressBin, 2)

		newAddresses := maskedAddressToAddresses(maskedAddress)

		for _, addr := range newAddresses {
			intAddr := utils.BinStrToInt(addr)
			mem[intAddr] = value
		}
	}

	var sum int = 0
	for _, v := range mem {
		sum += v
	}
	fmt.Printf("Part Two: %d\n", sum)
}

func applyMask(mask string, bin string, version int) string {
	// If the binary is smaller than the mask, pad it with zeroes
	if len(bin) < len(mask) {
		diff := len(mask) - len(bin)
		bin = strings.Repeat("0", diff) + bin
	}

	maskRunes := []rune(mask)
	binRunes := []rune(bin)

	for i, maskRune := range maskRunes {
		if (maskRune == 'X' && version == 1) || (maskRune == '0' && version == 2) {
			continue
		}

		binRunes[i] = maskRune
	}

	return string(binRunes)
}

// Generate the addresses based on a masked address.
// Example:
// 000000000000000000000000000000X1101X generates list containing:
//     - 000000000000000000000000000000011010
//     - 000000000000000000000000000000011011
//     - 000000000000000000000000000000111010
//     - 000000000000000000000000000000111011
func maskedAddressToAddresses(masked string) []string {
	addresses := []string{}
	maskedRunes := []rune(masked)
	xIdxs := []int{}

	for i, r := range maskedRunes {
		if r == 'X' {
			xIdxs = append(xIdxs, i)
		}
	}

	combinations := utils.SliceCombinations([]string{"0", "1"}, len(xIdxs))
	for _, combination := range combinations {
		newAddr := maskedRunes

		for i, idx := range xIdxs {
			c := []rune(combination[i])
			newAddr[idx] = c[0]
		}

		addresses = append(addresses, string(newAddr))
	}

	return addresses
}

func matchMask(input string) string {
	matchMask := maskPattern.FindAllStringSubmatch(input, -1)
	if matchMask == nil {
		return ""
	}

	return matchMask[0][1]
}

func matchMemOverride(input string) (int, int) {
	match := memInputPattern.FindAllStringSubmatch(input, -1)
	if match == nil {
		return -1, -1
	}

	memPos, _ := strconv.Atoi(match[0][1])
	value, _ := strconv.Atoi(match[0][2])

	return memPos, value
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}
