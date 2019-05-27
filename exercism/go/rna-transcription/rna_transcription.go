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

// benchmark results:
//
// BenchmarkRNATranscription-8   	10000000	       146 ns/op	      16 B/op	       5 allocs/op
// PASS
// ok  	github.com/little-dude/exercism/go/rna-transcription	1.633s
//
// Note that there is a simpler solution with strings.Replacer as
// found in
// https://exercism.io/tracks/go/exercises/rna-transcription/solutions/5fbbe4d003222e82656c3834
//
// var replacer = strings.NewReplacer(
// 	"G", "C",
// 	"C", "G",
// 	"T", "A",
// 	"A", "U")
//
// func ToRNA(input string) string {
// 	return replacer.Replace(input)
// }
//
// However benchmarks show that this is slower:
//
// BenchmarkRNATranscription-8   	10000000	       221 ns/op	      64 B/op	       6 allocs/op
// PASS
// ok  	github.com/little-dude/exercism/go/rna-transcription	2.468s
//
// More surprisingly, the solution with a simple byte buffer from
// https://exercism.io/tracks/go/exercises/rna-transcription/solutions/16da121be28d43e89ee1e8ae881b44e8
// is also slower, I'm not sure why, because afaict, there's also a
// single allocation when we make the byte buffer (unless the
// conversion to a string also allocates??)
//
// var translate = map[rune]byte{
// 	'G': 'C',
// 	'C': 'G',
// 	'T': 'A',
// 	'A': 'U',
// }
//
// // ToRNA - return DNA compliment
// func ToRNA(dna string) string {
// 	rna := make([]byte, len(dna))
// 	for i, c := range dna {
// 		rna[i] = translate[c]
// 	}
// 	return string(rna)
// }
//
// BenchmarkRNATranscription-8   	10000000	       191 ns/op	      32 B/op	       6 allocs/op
// PASS
// ok  	github.com/little-dude/exercism/go/rna-transcription	2.129s
