package hamming

import "errors"

func Distance(a, b string) (int, error) {
	if len(a) != len(b) {
		return -1, errors.New("strings must have the same length")
	}

	distance := 0
	i := 0
	for _, c := range a {
		if []rune(b)[i] != c {
			distance++
		}
		i++
	}
	return distance, nil
}
