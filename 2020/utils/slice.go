package utils

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
