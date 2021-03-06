package utils

import (
	"fmt"
	"strings"
)

func MatrixRotate(matrix [][]string) [][]string {
	newMatrix := make([][]string, len(matrix))

	for rowIdx := range newMatrix {
		newMatrix[rowIdx] = SliceReverse(MatrixCol(matrix, rowIdx))
	}

	return newMatrix
}

func MatrixCol(matrix [][]string, i int) []string {
	col := make([]string, len(matrix))
	for rowIdx, row := range matrix {
		col[rowIdx] = row[i]
	}

	return col
}

func MatrixStringify(matrix [][]string) string {
	var res strings.Builder

	for _, row := range matrix {
		for _, item := range row {
			fmt.Fprintf(&res, "%s ", item)
		}
		fmt.Fprint(&res, "\n")
	}

	return res.String()
}

func MatrixLookup(matrix [][]string, coords [][2]int) []string {
	res := []string{}
	for _, coord := range coords {
		res = append(res, matrix[coord[0]][coord[1]])
	}

	return res
}

func MatrixCount(matrix [][]string) map[string]int {
	result := map[string]int{}

	for _, row := range matrix {
		for _, item := range row {
			if _, ok := result[item]; !ok {
				result[item] = 0
			}

			result[item]++
		}
	}

	return result
}

func MatrixCountItem(matrix [][]string, value string) int {
	items := 0
	for _, row := range matrix {
		for _, item := range row {
			if item == value {
				items++
			}
		}
	}

	return items
}

func MatrixFlip(matrix [][]string) [][]string {
	newMatrix := make([][]string, len(matrix))

	for rowIdx, row := range matrix {
		newRow := make([]string, len(row))
		for i, j := 0, len(row)-1; i < j; i, j = i+1, j-1 {
			newRow[i], newRow[j] = row[j], row[i]
		}
		newMatrix[rowIdx] = newRow
	}

	return newMatrix
}

func MatrixRemove(matrix [][]string, item string) [][]string {
	for i, row := range matrix {
		matrix[i] = SliceRemove(row, item)
	}

	return matrix
}

func MatrixDeepCopy(matrix [][]string) [][]string {
	newMatrix := make([][]string, len(matrix))
	for i, v := range matrix {
		newMatrix[i] = make([]string, len(v))
		copy(newMatrix[i], v)
	}

	return newMatrix
}
