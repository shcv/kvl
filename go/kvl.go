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

import (
	"fmt"
	"strings"
)

// Config holds parser configuration extracted from a KVL header.
type Config struct {
	Separator   string
	Version     string
	ListMarkers string
	Options     map[string]string
	Strict      bool
	Diagnostics []Diagnostic
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

// Diagnostic represents a warning or error emitted during parsing.
type Diagnostic struct {
	Severity string // "warning" or "error"
	Code     string // e.g. "W001", "W002"
	Message  string
	Line     int
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
// Multiline string values are trimmed (leading newline stripped, dedented).
func Loads(text string) (map[string]any, error) {
	root, _, err := parse(text)
	if err != nil {
		return nil, err
	}
	result := root.toCompactedMap()
	trimMultilineValues(result)
	return result, nil
}

// Compact converts a categorical map[string]any to a compacted form.
// Categorical structures (maps where all values are empty maps) become
// []string lists. Single-entry categoricals become plain strings.
// Note: Since Go maps are unordered, list element order from Compact()
// is non-deterministic. Use Loads() for order-preserving parsing.
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
// Note: Since Go maps are unordered, the result order is non-deterministic.
// Use LoadsMerge for order-preserving merge with compacted output.
func Merge(a, b map[string]any) map[string]any {
	return mergeMap(a, b)
}

// LoadsMerge parses two KVL texts, merges them, and returns a compacted
// result preserving insertion order (from the node tree, not Go maps).
// Multiline string values are trimmed (leading newline stripped, dedented).
func LoadsMerge(text1, text2 string) (map[string]any, error) {
	root1, _, err := parse(text1)
	if err != nil {
		return nil, err
	}
	root2, _, err := parse(text2)
	if err != nil {
		return nil, err
	}
	root1.mergeFrom(root2)
	result := root1.toCompactedMap()
	trimMultilineValues(result)
	return result, nil
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
	return MarshalStringWithConfig(data, DefaultConfig(), false)
}

// MarshalStringWithConfig serializes a map[string]any using the provided config.
// When includeHeader is true, it prepends a matching KVL header.
func MarshalStringWithConfig(data map[string]any, cfg Config, includeHeader bool) (string, error) {
	body, err := serialize(data, cfg)
	if err != nil {
		return "", err
	}
	if !includeHeader {
		return body, nil
	}
	return generateHeader(cfg) + "\n" + body, nil
}

func generateHeader(cfg Config) string {
	parts := []string{"#" + cfg.Separator, "kvl"}
	version := cfg.Version
	if version == "" {
		version = "1.0"
	}
	parts = append(parts, version)
	if cfg.ListMarkers != "" {
		parts = append(parts, cfg.ListMarkers)
	}
	if cfg.Strict {
		parts = append(parts, "strict")
	}
	return strings.Join(parts, " ")
}
