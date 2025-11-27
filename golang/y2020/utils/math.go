package utils

import "math"

func DegreeToRadians(degrees int) float64 {
	return float64(degrees) * (math.Pi / 180.0)
}
