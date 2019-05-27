// Package proverb provide a function to generate proverbs from a list
// of words.
package proverb

import "fmt"

// Proverb returns the proverb's sentences for the given words.
func Proverb(rhyme []string) []string {
	result := make([]string, 0, len(rhyme))
	if len(rhyme) < 1 {
		return result
	}

	previous := rhyme[0]
	for _, s := range rhyme[1:] {
		result = append(result, fmt.Sprintf("For want of a %s the %s was lost.", previous, s))
		previous = s
	}
	return append(result, fmt.Sprintf("And all for the want of a %s.", rhyme[0]))
}
