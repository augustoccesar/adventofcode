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

// Note: Anything related to index consider the current visualization of the circle.
// If the circle is updated, need to find the indexes again
type SliceIntCircular struct {
	data []int
}

func NewSliceIntCircular(data []int) SliceIntCircular {
	return SliceIntCircular{data: SliceIntCopy(data)}
}

// Inclusive
func (s *SliceIntCircular) PopFromIdx(fromIdx, amount int) []int {
	dataCpy := SliceIntCopy(s.data)
	poppedData := make([]int, amount)
	pdi := 0 // popped data index

	newData := make([]int, len(s.data)-amount)
	ndi := 0 // new data index

	looped := false
	start := fromIdx
	i := start

	for !looped {
		if pdi < len(poppedData) {
			poppedData[pdi] = dataCpy[i]
			pdi++
		} else {
			newData[ndi] = dataCpy[i]
			ndi++
		}

		i = (i + 1) % len(dataCpy)

		if i == start {
			looped = true
		}
	}

	s.data = newData
	return poppedData
}

func (s *SliceIntCircular) InsertAt(atIdx int, insert []int) {
	s.data = SliceIntInsert(s.data, atIdx, insert)
}

func (s *SliceIntCircular) Get(index int) int {
	return s.data[index]
}

func (s *SliceIntCircular) Find(item int) int {
	return SliceFind(s.data, item)
}

func (s *SliceIntCircular) Loop(from int, function func(int, int)) {
	looped := false
	start := from
	i := start

	for !looped {
		function(i, s.data[i]) // NOTE: Keep an eye for reference items

		i = (i + 1) % len(s.data)

		if i == start {
			looped = true
		}
	}
}

func (s *SliceIntCircular) Tail(fromIdx int) []int {
	tail := make([]int, len(s.data)-1)
	i := 0
	s.Loop(fromIdx, func(idx, item int) {
		if idx != fromIdx {
			tail[i] = item
			i++
		}
	})

	return tail
}

func (s *SliceIntCircular) Len() int {
	return len(s.data)
}
