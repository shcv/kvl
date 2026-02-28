// Package kvl implements the KVL (Key-Value Language) configuration format.
//
// KVL is a minimalist configuration format with clean syntax, composable
// merge operations, and mathematical soundness (associative merging).
//
// Two API tiers are provided:
//
//   - Parse/ParseString: Low-level categorical representation (all strings, no type inference)
//   - Loads: High-level compacted representation (lists preserved in insertion order)
package kvl

import "fmt"

// Config holds parser configuration extracted from a KVL header.
type Config struct {
	Separator   string
	Version     string
	ListMarkers string
	Options     map[string]string
}

// DefaultConfig returns a Config with default settings.
func DefaultConfig() Config {
	return Config{
		Separator: "=",
		Options:   map[string]string{},
	}
}

// ParseError represents a parsing error with location information.
type ParseError struct {
	Line    int
	Message string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("line %d: %s", e.Line, e.Message)
}

// Parse parses KVL text into a categorical map[string]any.
// All values are strings. Repeated keys create nested categorical structures.
// For example, repeated "tags = web" and "tags = api" produce:
//
//	{"tags": {"web": {}, "api": {}}}
func Parse(data []byte) (map[string]any, error) {
	return ParseString(string(data))
}

// ParseString parses a KVL string into a categorical map[string]any.
func ParseString(text string) (map[string]any, error) {
	root, _, err := parse(text)
	if err != nil {
		return nil, err
	}
	return root.toMap(), nil
}

// Loads parses KVL text into a compacted map[string]any.
// Categorical structures are converted to lists preserving insertion order.
// Single-value categoricals become plain strings.
func Loads(text string) (map[string]any, error) {
	root, _, err := parse(text)
	if err != nil {
		return nil, err
	}
	result := root.toCompacted()
	if m, ok := result.(map[string]any); ok {
		return m, nil
	}
	return map[string]any{}, nil
}

// Compact converts a categorical map[string]any to a compacted form.
// Categorical structures (maps where all values are empty maps) become
// sorted []string lists. Single-entry categoricals become plain strings.
// Since Go maps are unordered, list order is alphabetical.
func Compact(data map[string]any) map[string]any {
	return compactMap(data)
}

// Expand converts a compacted map[string]any back to categorical form.
// Strings become {s: {}}, []string become {s1: {}, s2: {}, ...}.
func Expand(data map[string]any) map[string]any {
	return expandMap(data)
}

// Merge performs an associative merge of two categorical maps.
// Both maps are merged recursively: matching keys have their children merged.
// Merge(Merge(A, B), C) == Merge(A, Merge(B, C))
func Merge(a, b map[string]any) map[string]any {
	return mergeMap(a, b)
}

// Marshal serializes a map[string]any to KVL text using the default separator "=".
func Marshal(data map[string]any) ([]byte, error) {
	s, err := MarshalString(data)
	if err != nil {
		return nil, err
	}
	return []byte(s), nil
}

// MarshalString serializes a map[string]any to a KVL string.
func MarshalString(data map[string]any) (string, error) {
	return serialize(data, "="), nil
}
