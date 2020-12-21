package utils

func RemoveFromAll(m *map[string][]string, value string) {
	nm := *m

	for k, v := range nm {
		newList := []string{}
		for _, item := range v {
			if item != value {
				newList = append(newList, item)
			}
		}
		nm[k] = newList
	}

	*m = nm
}

func MapKeys(m map[string]string) []string {
	res := make([]string, len(m))
	i := 0
	for k := range m {
		res[i] = k
		i++
	}

	return res
}
