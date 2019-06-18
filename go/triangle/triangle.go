// Package triangle provides a helpers to work with triangles
package triangle

import "math"

// Kind represents a type of triangle.
type Kind int

const (
	// NaT (Not a Triangle) is for invalid triangles
	NaT Kind = iota
	// Equ represents a triangle that is equilateral
	Equ
	// Iso represents a triangle that is isosceles
	Iso
	// Sca represents a triangle that is scalene
	Sca
)

// KindFromSides returns the kind of triangle that correspond to the
// given triangle dimensions a, b, c.
func KindFromSides(a, b, c float64) Kind {
	if !isValidTriangle(a, b, c) {
		return NaT
	}
	if a == b && a == c {
		return Equ
	}
	if a == b || a == c || b == c {
		return Iso
	}
	return Sca
}

func isValidTriangle(a, b, c float64) bool {
	p := a * b * c
	return !(math.IsInf(p, 0) || math.IsNaN(p) || p == 0) && a+b >= c && a+c >= b && b+c >= a
}
