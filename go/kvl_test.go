package kvl

import (
	"encoding/json"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"testing"
)

// loadFixture reads a .kvl file and its .expected.json file.
func loadFixture(t *testing.T, dir, name string) (string, map[string]any) {
	t.Helper()
	kvlPath := filepath.Join("..", "fixtures", dir, name+".kvl")
	jsonPath := filepath.Join("..", "fixtures", dir, name+".expected.json")

	kvlData, err := os.ReadFile(kvlPath)
	if err != nil {
		t.Fatalf("failed to read %s: %v", kvlPath, err)
	}

	jsonData, err := os.ReadFile(jsonPath)
	if err != nil {
		t.Fatalf("failed to read %s: %v", jsonPath, err)
	}

	var expected map[string]any
	if err := json.Unmarshal(jsonData, &expected); err != nil {
		t.Fatalf("failed to parse expected JSON: %v", err)
	}

	return string(kvlData), expected
}

// normalizeJSON converts all numeric types to float64 for comparison
// and normalizes slice types.
func normalizeJSON(v any) any {
	switch val := v.(type) {
	case map[string]any:
		result := make(map[string]any, len(val))
		for k, v := range val {
			result[k] = normalizeJSON(v)
		}
		return result
	case []any:
		result := make([]any, len(val))
		for i, v := range val {
			result[i] = normalizeJSON(v)
		}
		return result
	case []string:
		result := make([]any, len(val))
		for i, s := range val {
			result[i] = s
		}
		return result
	case float64:
		return val
	case int:
		return float64(val)
	default:
		return val
	}
}

func jsonEqual(a, b any) bool {
	return reflect.DeepEqual(normalizeJSON(a), normalizeJSON(b))
}

// jsonEqualUnordered is like jsonEqual but sorts string slices before comparison.
// Use when comparing results from Compact (which produces sorted lists from unordered maps).
func jsonEqualUnordered(a, b any) bool {
	return reflect.DeepEqual(sortedNormalize(a), sortedNormalize(b))
}

func sortedNormalize(v any) any {
	switch val := v.(type) {
	case map[string]any:
		result := make(map[string]any, len(val))
		for k, v := range val {
			result[k] = sortedNormalize(v)
		}
		return result
	case []any:
		strs := make([]string, 0, len(val))
		allStrings := true
		for _, item := range val {
			if s, ok := item.(string); ok {
				strs = append(strs, s)
			} else {
				allStrings = false
				break
			}
		}
		if allStrings && len(strs) > 0 {
			sort.Strings(strs)
			result := make([]any, len(strs))
			for i, s := range strs {
				result[i] = s
			}
			return result
		}
		result := make([]any, len(val))
		for i, v := range val {
			result[i] = sortedNormalize(v)
		}
		return result
	case []string:
		sorted := make([]string, len(val))
		copy(sorted, val)
		sort.Strings(sorted)
		result := make([]any, len(sorted))
		for i, s := range sorted {
			result[i] = s
		}
		return result
	case float64:
		return val
	case int:
		return float64(val)
	default:
		return val
	}
}

// --- Core fixture tests ---

func TestCoreSimple(t *testing.T) {
	kvlText, expected := loadFixture(t, "core", "simple")
	result, err := Loads(kvlText)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if !jsonEqual(result, expected) {
		t.Errorf("mismatch\ngot:  %v\nwant: %v", toJSON(result), toJSON(expected))
	}
}

func TestCoreNested(t *testing.T) {
	kvlText, expected := loadFixture(t, "core", "nested")
	result, err := Loads(kvlText)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if !jsonEqual(result, expected) {
		t.Errorf("mismatch\ngot:  %v\nwant: %v", toJSON(result), toJSON(expected))
	}
}

// --- Valid fixture tests ---

func TestValidSimple(t *testing.T) {
	kvlText, expected := loadFixture(t, "valid", "simple")
	result, err := Loads(kvlText)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if !jsonEqual(result, expected) {
		t.Errorf("mismatch\ngot:  %v\nwant: %v", toJSON(result), toJSON(expected))
	}
}

func TestValidNested(t *testing.T) {
	kvlText, expected := loadFixture(t, "valid", "nested")
	result, err := Loads(kvlText)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if !jsonEqual(result, expected) {
		t.Errorf("mismatch\ngot:  %v\nwant: %v", toJSON(result), toJSON(expected))
	}
}

func TestValidCategorical(t *testing.T) {
	kvlText, expected := loadFixture(t, "valid", "categorical")
	result, err := Loads(kvlText)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if !jsonEqual(result, expected) {
		t.Errorf("mismatch\ngot:  %v\nwant: %v", toJSON(result), toJSON(expected))
	}
}

func TestValidComments(t *testing.T) {
	kvlText, expected := loadFixture(t, "valid", "comments")
	result, err := Loads(kvlText)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if !jsonEqual(result, expected) {
		t.Errorf("mismatch\ngot:  %v\nwant: %v", toJSON(result), toJSON(expected))
	}
}

func TestValidTypes(t *testing.T) {
	kvlText, expected := loadFixture(t, "valid", "types")
	result, err := Loads(kvlText)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if !jsonEqual(result, expected) {
		t.Errorf("mismatch\ngot:  %v\nwant: %v", toJSON(result), toJSON(expected))
	}
}

// --- Parse (categorical) tests ---

func TestParseCategorical(t *testing.T) {
	input := "tags = web\ntags = api\ntags = production\n"
	result, err := ParseString(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	tags, ok := result["tags"].(map[string]any)
	if !ok {
		t.Fatalf("tags should be map, got %T", result["tags"])
	}

	for _, key := range []string{"web", "api", "production"} {
		if _, ok := tags[key]; !ok {
			t.Errorf("missing key %q in tags", key)
		}
	}
}

// --- Merge tests ---

func TestMergeAssociativity(t *testing.T) {
	a := map[string]any{
		"x": map[string]any{"1": map[string]any{}},
		"y": map[string]any{"a": map[string]any{}},
	}
	b := map[string]any{
		"x": map[string]any{"2": map[string]any{}},
		"z": map[string]any{"b": map[string]any{}},
	}
	c := map[string]any{
		"x": map[string]any{"3": map[string]any{}},
		"y": map[string]any{"c": map[string]any{}},
	}

	ab_c := Merge(Merge(a, b), c)
	a_bc := Merge(a, Merge(b, c))

	if !reflect.DeepEqual(ab_c, a_bc) {
		t.Errorf("merge not associative\n(A+B)+C = %v\nA+(B+C) = %v", toJSON(ab_c), toJSON(a_bc))
	}
}

func TestMergeFixtures(t *testing.T) {
	baseText, _ := loadFixtureKVL(t, filepath.Join("merge", "basic"), "base")
	overlayText, _ := loadFixtureKVL(t, filepath.Join("merge", "basic"), "overlay")

	jsonPath := filepath.Join("..", "fixtures", "merge", "basic", "expected.json")
	jsonData, err := os.ReadFile(jsonPath)
	if err != nil {
		t.Fatalf("failed to read expected: %v", err)
	}
	var expected map[string]any
	if err := json.Unmarshal(jsonData, &expected); err != nil {
		t.Fatalf("failed to parse expected: %v", err)
	}

	baseCat, err := ParseString(baseText)
	if err != nil {
		t.Fatalf("parse base: %v", err)
	}
	overlayCat, err := ParseString(overlayText)
	if err != nil {
		t.Fatalf("parse overlay: %v", err)
	}

	merged := Merge(baseCat, overlayCat)
	compacted := Compact(merged)

	// Compact on unordered maps produces sorted lists; expected may have insertion order.
	// Use unordered comparison.
	if !jsonEqualUnordered(compacted, expected) {
		t.Errorf("merge mismatch\ngot:  %v\nwant: %v", toJSON(compacted), toJSON(expected))
	}
}

func loadFixtureKVL(t *testing.T, dir, name string) (string, error) {
	t.Helper()
	kvlPath := filepath.Join("..", "fixtures", dir, name+".kvl")
	data, err := os.ReadFile(kvlPath)
	if err != nil {
		t.Fatalf("failed to read %s: %v", kvlPath, err)
	}
	return string(data), nil
}

// --- Compact/Expand round-trip ---

func TestCompactExpand(t *testing.T) {
	categorical := map[string]any{
		"name": map[string]any{"Alice": map[string]any{}},
		"tags": map[string]any{
			"web":  map[string]any{},
			"api":  map[string]any{},
			"prod": map[string]any{},
		},
		"server": map[string]any{
			"host": map[string]any{"localhost": map[string]any{}},
			"port": map[string]any{"8080": map[string]any{}},
		},
	}

	compacted := Compact(categorical)

	// name should be string
	if name, ok := compacted["name"].(string); !ok || name != "Alice" {
		t.Errorf("name: got %v, want Alice", compacted["name"])
	}

	// tags should be sorted []string
	tags, ok := compacted["tags"].([]string)
	if !ok {
		t.Fatalf("tags should be []string, got %T", compacted["tags"])
	}
	sort.Strings(tags)
	expectedTags := []string{"api", "prod", "web"}
	if !reflect.DeepEqual(tags, expectedTags) {
		t.Errorf("tags: got %v, want %v", tags, expectedTags)
	}

	// Expand back
	expanded := Expand(compacted)

	// Check structure
	nameMap, ok := expanded["name"].(map[string]any)
	if !ok {
		t.Fatalf("expanded name should be map, got %T", expanded["name"])
	}
	if _, ok := nameMap["Alice"]; !ok {
		t.Error("expanded name missing Alice")
	}
}

// --- Escape sequences ---

func TestEscapeSequences(t *testing.T) {
	input := `equation = x\=y+z`
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if v := result["equation"]; v != "x=y+z" {
		t.Errorf("equation: got %q, want %q", v, "x=y+z")
	}
}

func TestEscapeInKey(t *testing.T) {
	input := `a\=b = value`
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if v := result["a=b"]; v != "value" {
		t.Errorf("key a=b: got %q, want %q", v, "value")
	}
}

// --- Header parsing ---

func TestHeaderParsing(t *testing.T) {
	tests := []struct {
		line string
		sep  string
		ver  string
		lm   string
	}{
		{"#= kvl 1.0", "=", "1.0", ""},
		{"#: kvl 1.0", ":", "1.0", ""},
		{"#-> kvl 1.0", "->", "1.0", ""},
		{"#:= kvl 1.0", ":=", "1.0", ""},
		{"#= kvl 1.0 -", "=", "1.0", "-"},
		{"#: kvl 1.0 -+*", ":", "1.0", "-+*"},
	}

	for _, tt := range tests {
		cfg, ok := tryParseHeader(tt.line)
		if !ok {
			t.Errorf("failed to parse header %q", tt.line)
			continue
		}
		if cfg.Separator != tt.sep {
			t.Errorf("header %q: sep got %q, want %q", tt.line, cfg.Separator, tt.sep)
		}
		if cfg.Version != tt.ver {
			t.Errorf("header %q: version got %q, want %q", tt.line, cfg.Version, tt.ver)
		}
		if cfg.ListMarkers != tt.lm {
			t.Errorf("header %q: markers got %q, want %q", tt.line, cfg.ListMarkers, tt.lm)
		}
	}
}

// --- List markers ---

func TestListMarkers(t *testing.T) {
	input := "#: kvl 1.0 -\ndeps:\n  - react\n  - webpack\n"
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	deps, ok := result["deps"]
	if !ok {
		t.Fatal("missing deps key")
	}
	depsList, ok := deps.([]string)
	if !ok {
		t.Fatalf("deps should be []string, got %T: %v", deps, deps)
	}
	expected := []string{"react", "webpack"}
	if !reflect.DeepEqual(depsList, expected) {
		t.Errorf("deps: got %v, want %v", depsList, expected)
	}
}

// --- Edge cases ---

func TestEmptyInput(t *testing.T) {
	result, err := Loads("")
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(result) != 0 {
		t.Errorf("empty input should produce empty map, got %v", result)
	}
}

func TestEmptyValue(t *testing.T) {
	input := "key =\n"
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if v := result["key"]; v != "" {
		t.Errorf("empty value: got %q, want empty string", v)
	}
}

func TestDeepNesting(t *testing.T) {
	input := "a =\n    b =\n        c =\n            d = deep\n"
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	a, ok := result["a"].(map[string]any)
	if !ok {
		t.Fatalf("a should be map, got %T", result["a"])
	}
	b, ok := a["b"].(map[string]any)
	if !ok {
		t.Fatalf("b should be map, got %T", a["b"])
	}
	c, ok := b["c"].(map[string]any)
	if !ok {
		t.Fatalf("c should be map, got %T", b["c"])
	}
	if d := c["d"]; d != "deep" {
		t.Errorf("d: got %q, want %q", d, "deep")
	}
}

func TestCommentsAreKeys(t *testing.T) {
	input := "/= first comment\n/= second comment\nkey = value\n"
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	comments, ok := result["/"]
	if !ok {
		t.Fatal("missing / key for comments")
	}

	commentList, ok := comments.([]string)
	if !ok {
		t.Fatalf("/ should be []string, got %T", comments)
	}
	expected := []string{"first comment", "second comment"}
	if !reflect.DeepEqual(commentList, expected) {
		t.Errorf("comments: got %v, want %v", commentList, expected)
	}
}

func TestQuotedValuePreserved(t *testing.T) {
	input := `quoted = "hello world"` + "\n"
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if v := result["quoted"]; v != `"hello world"` {
		t.Errorf("quoted: got %q, want %q", v, `"hello world"`)
	}
}

// --- Serialization ---

func TestSerializeSimple(t *testing.T) {
	data := map[string]any{
		"name": "Alice",
		"port": "8080",
	}
	s, err := MarshalString(data)
	if err != nil {
		t.Fatalf("marshal error: %v", err)
	}

	// Re-parse to verify round-trip
	result, err := Loads(s)
	if err != nil {
		t.Fatalf("re-parse error: %v", err)
	}
	if !jsonEqual(result, data) {
		t.Errorf("round-trip mismatch\ngot:  %v\nwant: %v", result, data)
	}
}

func TestSerializeNested(t *testing.T) {
	data := map[string]any{
		"server": map[string]any{
			"host": "localhost",
			"port": "8080",
		},
	}
	s, err := MarshalString(data)
	if err != nil {
		t.Fatalf("marshal error: %v", err)
	}

	result, err := Loads(s)
	if err != nil {
		t.Fatalf("re-parse error: %v", err)
	}
	if !jsonEqual(result, data) {
		t.Errorf("round-trip mismatch\ngot:  %v\nwant: %v", result, data)
	}
}

// --- Custom separator ---

func TestCustomSeparator(t *testing.T) {
	input := "#: kvl 1.0\nhost: localhost\nport: 8080\n"
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if v := result["host"]; v != "localhost" {
		t.Errorf("host: got %q, want %q", v, "localhost")
	}
	if v := result["port"]; v != "8080" {
		t.Errorf("port: got %q, want %q", v, "8080")
	}
}

func TestArrowSeparator(t *testing.T) {
	input := "#-> kvl 1.0\nhost -> localhost\nport -> 8080\n"
	result, err := Loads(input)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if v := result["host"]; v != "localhost" {
		t.Errorf("host: got %q, want %q", v, "localhost")
	}
	if v := result["port"]; v != "8080" {
		t.Errorf("port: got %q, want %q", v, "8080")
	}
}

// helper
func toJSON(v any) string {
	b, _ := json.MarshalIndent(v, "", "  ")
	return string(b)
}
