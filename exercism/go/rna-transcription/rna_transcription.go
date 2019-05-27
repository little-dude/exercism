package strand

import "strings"

// ToRNA converts the given DNA string into the corresponding RNA.
func ToRNA(dna string) string {
	var result strings.Builder
	result.Grow(len(dna))
	for _, c := range dna {
		switch c {
		case 'G':
			result.WriteRune('C')
		case 'C':
			result.WriteRune('G')
		case 'T':
			result.WriteRune('A')
		case 'A':
			result.WriteRune('U')
		}
	}
	return result.String()
}
