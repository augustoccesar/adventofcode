package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

var memInputPattern = regexp.MustCompile(`mem\[(\d+)\] = (\d+)`)
var maskPattern = regexp.MustCompile(`mask = (.+)`)

func partOne() {
	input := strings.Split(readInput(), "\n")

	currentMask := ""
	mem := map[int]int64{}
	for _, item := range input {
		matchMask := matchMask(item)
		if matchMask != "" {
			currentMask = matchMask
			continue
		}

		memAddr, value := matchMemOverride(item)

		valueBinStr := intToBinStr(value)

		newValueBinStr := applyMask(currentMask, valueBinStr, 1)
		newValueInt := binStrToInt(newValueBinStr)

		mem[memAddr] = newValueInt
	}

	var sum int64 = 0
	for _, v := range mem {
		sum += v
	}
	fmt.Printf("Part One: %d\n", sum)
}

func partTwo() {
	input := strings.Split(readInput(), "\n")

	currentMask := ""
	mem := map[int64]int{}
	for _, item := range input {
		matchMask := matchMask(item)
		if matchMask != "" {
			currentMask = matchMask
			continue
		}

		memAddr, value := matchMemOverride(item)

		addressBin := intToBinStr(memAddr)
		maskedAddress := applyMask(currentMask, addressBin, 2)

		newAddresses := maskedAddressToAddresses(maskedAddress)

		for _, addr := range newAddresses {
			intAddr := binStrToInt(addr)
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

	combinations := combinations([]string{"0", "1"}, len(xIdxs))
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

func intToBinStr(in int) string {
	return fmt.Sprintf("%s", strconv.FormatInt(int64(in), 2))
}

func binStrToInt(binStr string) int64 {
	v, err := strconv.ParseInt(binStr, 2, 64)
	if err != nil {
		// Only panic here because I want to know if this goes wrong.
		// But it shouldn't reach here since the input is well controlled
		panic(err)
	}

	return v
}

// Generate combinations for an X size using a set of strings
// Example:
// []int{"0", "1"} and size 2 generates list containing:
//    - []int{"0", "0"}
//    - []int{"0", "1"}
//    - []int{"1", "0"}
//    - []int{"1", "1"}
func combinations(set []string, size int) [][]string {
	result := [][]string{}
	for _, item := range set {
		if size > 1 {
			for _, item2 := range combinations(set, size-1) {
				n := append([]string{item}, item2...)
				result = append(result, n)
			}
			continue
		}

		result = append(result, []string{item})
	}

	return result
}

// --------------------------------------------------------------------------------------------------------------------

func main() {
	partOne()
	partTwo()
}

func readInput() string {
	input, err := ioutil.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	return string(input)
}
