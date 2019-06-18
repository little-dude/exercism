// Package leap provides helpers to deal with leap years.
package leap

// IsLeapYear returns whether year is a leap year or not.
func IsLeapYear(year int) bool {
	return (year%4 == 0) && ((year%100 != 0) || (year%400 == 0))
}
