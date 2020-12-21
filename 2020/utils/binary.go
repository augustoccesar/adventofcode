package utils

import (
	"fmt"
	"strconv"
)

func IntToBinStr(in int) string {
	return fmt.Sprintf("%s", strconv.FormatInt(int64(in), 2))
}

func BinStrToInt(binStr string) int64 {
	v, err := strconv.ParseInt(binStr, 2, 64)
	if err != nil {
		// Only panic here because I want to know if this goes wrong.
		// But it shouldn't reach here since the input is well controlled
		panic(err)
	}

	return v
}
