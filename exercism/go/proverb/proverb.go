// Package proverb provide a function to generate proverbs from a list
// of words.
package proverb

import "fmt"

const (
	stanza = "For want of a %s the %s was lost."
	last   = "And all for the want of a %s."
)

// Proverb returns the proverb's sentences for the given words.
func Proverb(rhyme []string) []string {
	if len(rhyme) < 1 {
		return []string{}
	}

	result := make([]string, 0, len(rhyme))
	previous := rhyme[0]
	for _, s := range rhyme[1:] {
		result = append(result, fmt.Sprintf(stanza, previous, s))
		previous = s
	}
	return append(result, fmt.Sprintf(last, rhyme[0]))
}

// Benchmark:
//
// BenchmarkProverb-8   	  300000	      3454 ns/op	    1472 B/op	      51 allocs/op
// PASS
// ok  	github.com/little-dude/exercism/go/proverb	1.090s
