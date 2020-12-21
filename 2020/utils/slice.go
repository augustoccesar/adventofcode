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
