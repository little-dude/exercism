package space

import "fmt"

// Planet represents a planet.
type Planet string

var timeFactors = map[Planet]float64{
	"Earth":   1,
	"Mercury": 0.2408467,
	"Venus":   0.61519726,
	"Mars":    1.8808158,
	"Jupiter": 11.862615,
	"Saturn":  29.447498,
	"Uranus":  84.016846,
	"Neptune": 164.79132,
}

var earthYearInSeconds = float64(31557600)

// Age returns the age corresponding to seconds on the given planet.
func Age(seconds float64, planet Planet) float64 {
	if timeFactor, ok := timeFactors[planet]; ok {
		return (seconds / earthYearInSeconds) / timeFactor
	}
	panic(fmt.Sprintf("Uknown planet %s", planet))
}
