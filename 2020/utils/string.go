package utils

import "strconv"

func Atoi(str string) int {
	val, err := strconv.Atoi(str)
	if err != nil {
		panic(err)
	}

	return val
}

// There must be a better way of doing this xD
// replace replaces a string with a substring from defined points (both inclusive) on the input string
// Example:
//   input = "a b c d e f"
//   replace = "x"
//   start = 4
//   end = 6
//
//   result = "a b x e f"
func StringReplace(input string, replace string, start, end int) string {
	inputRunes := []rune(input)
	replaceRunes := []rune(replace)

	newSize := len(inputRunes) + len(replace) - ((end - start) + 1)

	result := make([]rune, newSize)

	for i, j := 0, 0; i < len(inputRunes); i, j = i+1, j+1 {
		if i != start {
			result[j] = inputRunes[i]
			continue
		}

		if i == start {
			for l := 0; l < len(replaceRunes); l++ {
				result[i+l] = replaceRunes[l]
			}

			j += len(replaceRunes) - 1
			i = end
		}
	}

	res := string(result)
	return res
}
