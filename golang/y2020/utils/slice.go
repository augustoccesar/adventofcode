package utils

import (
	"math"
	"strconv"
)

func SliceCopy(slice []string) []string {
	newSlice := make([]string, len(slice))
	copy(newSlice, slice)

	return newSlice
}

func SliceIntCopy(slice []int) []int {
	newSlice := make([]int, len(slice))
	copy(newSlice, slice)

	return newSlice
}

func SliceIntersect(sliceA []string, sliceB []string) []string {
	res := []string{}
	for _, item := range sliceA {
		if SliceContains(sliceB, item) {
			res = append(res, item)
		}
	}
	return res
}

func SliceContains(slice []string, value string) bool {
	for _, item := range slice {
		if item == value {
			return true
		}
	}

	return false
}

func SliceFind(slice []int, value int) int {
	for i, item := range slice {
		if item == value {
			return i
		}
	}

	return -1
}

func SliceSubslice(slice []int, start int, end int) (selected []int, remaining []int) {
	newSlice := SliceIntCopy(slice)

	selected = newSlice[start:end]
	remaining = append(remaining, newSlice[0:start]...)
	remaining = append(remaining, newSlice[end:]...)

	return
}

func SliceIntMaxMin(slice []int) (max, min int) {
	max = math.MinInt64
	min = math.MaxInt64

	for _, item := range slice {
		if item > max {
			max = item
		}

		if item < min {
			min = item
		}
	}

	return
}

// Position is inclusive
func SliceIntInsert(original []int, position int, insertion []int) (newSlice []int) {
	originalCpy := SliceIntCopy(original)
	newSlice = make([]int, len(original)+len(insertion))

	for newIdx, origIdx, inIdx := 0, 0, 0; newIdx < len(newSlice); newIdx++ {
		if newIdx <= position || inIdx == len(insertion) {
			newSlice[newIdx] = originalCpy[origIdx]
			origIdx++
		} else {
			newSlice[newIdx] = insertion[inIdx]
			inIdx++
		}
	}

	return
}

func SliceReverse(slice []string) []string {
	for i, j := 0, len(slice)-1; i < j; i, j = i+1, j-1 {
		slice[i], slice[j] = slice[j], slice[i]
	}

	return slice
}

func SliceRemove(slice []string, item string) []string {
	newSlice := []string{}

	for _, value := range slice {
		if value != item {
			newSlice = append(newSlice, value)
		}
	}

	return newSlice
}

func SliceAll(slice []string, value string) bool {
	for _, item := range slice {
		if item != value {
			return false
		}
	}

	return true
}

// SliceCombinations Generate combinations for an X size using a slice of strings
// Example:
// []int{"0", "1"} and size 2 generates list containing:
//    - []int{"0", "0"}
//    - []int{"0", "1"}
//    - []int{"1", "0"}
//    - []int{"1", "1"}
func SliceCombinations(slice []string, size int) [][]string {
	result := [][]string{}
	for _, item := range slice {
		if size > 1 {
			for _, item2 := range SliceCombinations(slice, size-1) {
				n := append([]string{item}, item2...)
				result = append(result, n)
			}
			continue
		}

		result = append(result, []string{item})
	}

	return result
}

func SliceCount(slice []string) map[string]int {
	result := map[string]int{}

	for _, item := range slice {
		if _, ok := result[item]; !ok {
			result[item] = 0
		}

		result[item]++
	}

	return result
}

func SlicePopFront(slice []string) (item string, newSlice []string) {
	item, newSlice = slice[0], slice[1:]

	return
}

func SliceIntToStr(slice []int) []string {
	newSlice := make([]string, len(slice))
	for i, item := range slice {
		newSlice[i] = strconv.Itoa(item)
	}

	return newSlice
}

func SliceStrToInt(slice []string) []int {
	newSlice := make([]int, len(slice))
	for i, item := range slice {
		val, err := strconv.Atoi(item)
		if err != nil {
			panic(err)
		}

		newSlice[i] = val
	}

	return newSlice
}

func MakeRange(min, max int) []int {
	a := make([]int, max-min+1)
	for i := range a {
		a[i] = min + i
	}
	return a
}
