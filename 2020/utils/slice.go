package utils

func SliceIntersect(sliceA []string, sliceB []string) []string {
	res := []string{}
	for _, item := range sliceA {
		if Contains(sliceB, item) {
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

func SliceAll(slice []string, value string) bool {
	for _, item := range slice {
		if item != value {
			return false
		}
	}

	return true
}
