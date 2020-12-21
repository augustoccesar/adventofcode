package utils

import "io/ioutil"

func ReadFile(path string) string {
	input, err := ioutil.ReadFile(path)
	if err != nil {
		panic(err)
	}

	return string(input)
}
