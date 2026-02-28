package kvl

import (
	"fmt"
	"math"
	"strings"
)

// Security limits
const (
	maxInputSize      = 10 * 1024 * 1024 // 10MB
	maxRecursionDepth = 100
)

// indentMode tracks whether a file uses tabs or spaces for indentation.
type indentMode int

const (
	indentUnknown indentMode = iota
	indentSpaces
	indentTabs
)

// parse is the main entry point for parsing KVL text.
// Returns the root node and the config parsed from any header.
func parse(text string) (*node, Config, error) {
	if len(text) > maxInputSize {
		return nil, Config{}, fmt.Errorf("input exceeds maximum size of %d bytes", maxInputSize)
	}

	// Normalize CRLF to LF
	text = strings.ReplaceAll(text, "\r\n", "\n")
	// Strip any remaining standalone CR
	text = strings.ReplaceAll(text, "\r", "\n")

	lines := strings.Split(text, "\n")
	cfg := DefaultConfig()

	startLine := 0
	if len(lines) > 0 {
		if c, ok := tryParseHeader(lines[0]); ok {
			cfg = c
			startLine = 1
		}
	}

	root := newNode()
	mode := indentUnknown
	_, _, err := buildTree(root, lines, startLine, -1, &cfg, &mode, 0)
	if err != nil {
		return nil, cfg, err
	}

	unescapeNode(root, cfg.Separator)
	return root, cfg, nil
}

// emitDiagnostic adds a warning diagnostic, or returns an error if strict mode is enabled.
func emitDiagnostic(cfg *Config, code, message string, line int) error {
	if cfg.Strict {
		return &ParseError{Line: line, Message: fmt.Sprintf("[%s] %s", code, message)}
	}
	cfg.Diagnostics = append(cfg.Diagnostics, Diagnostic{
		Severity: "warning", Code: code, Message: message, Line: line,
	})
	return nil
}

// classifyBlock checks lines in the indented block starting at startLine
// (with indentation > parentIndent). It finds the minimum indent level
// among non-blank lines (base-level lines) and checks whether ALL or SOME
// of those base-level lines contain unescaped separators or list markers.
func classifyBlock(lines []string, startLine, parentIndent int, cfg *Config, mode *indentMode) (allSep bool, someSep bool) {
	// First pass: find minimum indent among non-blank lines in the block
	minIndent := math.MaxInt
	for i := startLine; i < len(lines); i++ {
		line := lines[i]
		if isBlank(line) {
			continue
		}
		indent, err := measureIndentNoEnforce(line)
		if err != nil || indent <= parentIndent {
			break
		}
		if indent < minIndent {
			minIndent = indent
		}
	}

	if minIndent == math.MaxInt {
		return false, false
	}

	// Second pass: check all base-level lines (those at minIndent)
	totalBase := 0
	sepCount := 0
	for i := startLine; i < len(lines); i++ {
		line := lines[i]
		if isBlank(line) {
			continue
		}
		indent, err := measureIndentNoEnforce(line)
		if err != nil || indent <= parentIndent {
			break
		}
		if indent != minIndent {
			continue
		}
		totalBase++
		content := strings.TrimLeft(line, " \t")
		hasSep := false
		// Check for list markers
		if cfg.ListMarkers != "" && len(content) >= 2 {
			for _, marker := range cfg.ListMarkers {
				if rune(content[0]) == marker && (content[1] == ' ' || content[1] == '\t') {
					hasSep = true
					break
				}
			}
		}
		if !hasSep && findUnescapedSep(content, cfg.Separator) >= 0 {
			hasSep = true
		}
		if hasSep {
			sepCount++
		}
	}

	if totalBase == 0 {
		return false, false
	}
	return sepCount == totalBase, sepCount > 0
}

// collectMultilineBlock collects all indented lines after a key with empty
// value as a single multiline string. Returns the value and next line index.
func collectMultilineBlock(lines []string, startLine, parentIndent int, mode *indentMode) (string, int) {
	var parts []string
	i := startLine

	for i < len(lines) {
		line := lines[i]
		if isBlank(line) {
			// Peek ahead: if there are more indented lines after blank, include blank
			j := i + 1
			for j < len(lines) && isBlank(lines[j]) {
				j++
			}
			if j < len(lines) {
				nextIndent, err := measureIndentNoEnforce(lines[j])
				if err == nil && nextIndent > parentIndent {
					for k := i; k < j; k++ {
						parts = append(parts, lines[k])
					}
					i = j
					continue
				}
			}
			break
		}
		indent, err := measureIndentNoEnforce(line)
		if err != nil || indent <= parentIndent {
			break
		}
		parts = append(parts, line)
		i++
	}

	if len(parts) == 0 {
		return "", startLine
	}

	// Build multiline value with leading newline (matches Python behavior for categorical)
	var b strings.Builder
	for _, p := range parts {
		b.WriteByte('\n')
		b.WriteString(p)
	}
	return b.String(), i
}

// buildTree recursively builds the node tree from lines.
// parentIndent is the indentation level of the parent block (-1 for root).
// Returns the next line index to process and the updated indent mode.
func buildTree(parent *node, lines []string, startLine, parentIndent int, cfg *Config, mode *indentMode, depth int) (int, indentMode, error) {
	if depth > maxRecursionDepth {
		return 0, *mode, &ParseError{Line: startLine + 1, Message: fmt.Sprintf("maximum nesting depth of %d exceeded", maxRecursionDepth)}
	}

	blockIndent := -1
	i := startLine

	for i < len(lines) {
		line := lines[i]

		// Skip blank lines
		if isBlank(line) {
			i++
			continue
		}

		indent, err := measureIndent(line, mode)
		if err != nil {
			return 0, *mode, &ParseError{Line: i + 1, Message: err.Error()}
		}

		// If indent <= parentIndent, we're done with this block
		if indent <= parentIndent {
			return i, *mode, nil
		}

		// Set block indent from first non-blank line
		if blockIndent == -1 {
			blockIndent = indent
		} else if indent != blockIndent {
			if indent < blockIndent {
				return i, *mode, nil
			}
			return 0, *mode, &ParseError{Line: i + 1, Message: "inconsistent indentation"}
		}

		content := strings.TrimLeft(line, " \t")
		key, value := parseLine(content, *cfg)

		if key == "" && value != "" {
			// Empty key with value (from list markers or "= value"):
			// Add value directly as categorical entry in parent.
			// Use addListEntry to preserve list structure for categorical output.
			parent.addListEntry(value, newNode())
			i++
		} else if value == "" {
			// Empty value: check if next indented lines are nested KVL or multiline text
			allSep, someSep := classifyBlock(lines, i+1, blockIndent, cfg, mode)

			if allSep {
				// All base-level lines have separators: nested KVL (re-parse)
				child := newNode()
				nextLine, _, err := buildTree(child, lines, i+1, blockIndent, cfg, mode, depth+1)
				if err != nil {
					return 0, *mode, err
				}

				if child.isEmpty() {
					parent.addEntry(key, newNode())
				} else {
					parent.addEntry(key, child)
				}
				i = nextLine
			} else {
				if someSep {
					// Some but not all base-level lines have separators: W002 warning
					if err := emitDiagnostic(cfg, "W002", "indented block has mixed separator and plain-text lines; treating as plain text", i+1); err != nil {
						return 0, *mode, err
					}
				}
				// Check for multiline value continuation
				multiValue, nextLine := collectMultilineBlock(lines, i+1, blockIndent, mode)
				if multiValue != "" {
					// Multiline value: create leaf
					leaf := newNode()
					leaf.addEntry(multiValue, newNode())
					parent.addEntry(key, leaf)
					i = nextLine
				} else {
					// Truly empty value
					parent.addEntry(key, newNode())
					i++
				}
			}
		} else {
			// Has a value — check for multiline continuation
			multiValue, nextLine := collectMultilineContinuation(lines, i, value, blockIndent, mode)

			if multiValue != value {
				// Continuation lines were collected: emit W001
				if err := emitDiagnostic(cfg, "W001", "valued key has indented continuation lines; treating as multiline value", i+1); err != nil {
					return 0, *mode, err
				}
			}

			leaf := newNode()
			leaf.addEntry(multiValue, newNode())
			parent.addEntry(key, leaf)
			i = nextLine
		}
	}

	return i, *mode, nil
}

// collectMultilineContinuation collects continuation lines that are indented
// deeper than the current line's block indent for a value that already has
// initial content. Returns the combined value and next line index.
func collectMultilineContinuation(lines []string, currentLine int, initialValue string, blockIndent int, mode *indentMode) (string, int) {
	i := currentLine + 1

	// Check if next non-blank line is more indented
	for i < len(lines) && isBlank(lines[i]) {
		i++
	}
	if i >= len(lines) {
		return initialValue, currentLine + 1
	}

	nextIndent, err := measureIndentNoEnforce(lines[i])
	if err != nil || nextIndent <= blockIndent {
		return initialValue, currentLine + 1
	}

	// Has continuation — collect all deeper-indented lines
	i = currentLine + 1
	var parts []string

	for i < len(lines) {
		line := lines[i]
		if isBlank(line) {
			// Peek ahead: only include blank lines if followed by deeper content
			j := i + 1
			for j < len(lines) && isBlank(lines[j]) {
				j++
			}
			if j < len(lines) {
				ni, nerr := measureIndentNoEnforce(lines[j])
				if nerr == nil && ni > blockIndent {
					for k := i; k < j; k++ {
						parts = append(parts, lines[k])
					}
					i = j
					continue
				}
			}
			break
		}
		indent, ierr := measureIndentNoEnforce(line)
		if ierr != nil || indent <= blockIndent {
			break
		}
		parts = append(parts, line)
		i++
	}

	if len(parts) == 0 {
		return initialValue, currentLine + 1
	}

	var b strings.Builder
	b.WriteString(initialValue)
	for _, p := range parts {
		b.WriteByte('\n')
		b.WriteString(p)
	}
	return b.String(), i
}

// measureIndent counts the effective indentation of a line and enforces
// strict tab/space mode.
func measureIndent(line string, mode *indentMode) (int, error) {
	indent := 0
	hasTabs := false
	hasSpaces := false

	for _, ch := range line {
		switch ch {
		case ' ':
			indent++
			hasSpaces = true
		case '\t':
			indent += 4
			hasTabs = true
		default:
			goto done
		}
	}
done:
	// Only enforce mode if there is actual indentation
	if indent > 0 {
		if hasTabs && hasSpaces {
			return 0, fmt.Errorf("mixed tabs and spaces in indentation")
		}
		if hasTabs {
			if *mode == indentSpaces {
				return 0, fmt.Errorf("tab indentation used in a file that uses spaces")
			}
			*mode = indentTabs
		}
		if hasSpaces {
			if *mode == indentTabs {
				return 0, fmt.Errorf("space indentation used in a file that uses tabs")
			}
			*mode = indentSpaces
		}
	}

	return indent, nil
}

// measureIndentNoEnforce counts effective indentation without enforcing
// tab/space mode. Used for lookahead checks.
func measureIndentNoEnforce(line string) (int, error) {
	indent := 0
	for _, ch := range line {
		switch ch {
		case ' ':
			indent++
		case '\t':
			indent += 4
		default:
			return indent, nil
		}
	}
	return indent, nil
}

// isBlank returns true if the line is empty or contains only whitespace.
func isBlank(line string) bool {
	return strings.TrimSpace(line) == ""
}

// parseLine splits a content line (already trimmed of leading indent) into key and value.
// It handles list markers and separator finding.
func parseLine(content string, cfg Config) (string, string) {
	// Check for list markers
	if cfg.ListMarkers != "" && len(content) >= 2 {
		for _, marker := range cfg.ListMarkers {
			if rune(content[0]) == marker && (content[1] == ' ' || content[1] == '\t') {
				// List marker: empty key, rest is value
				return "", strings.TrimLeft(content[2:], " \t")
			}
		}
	}

	// Find the unescaped separator
	sep := cfg.Separator
	pos := findUnescapedSep(content, sep)
	if pos < 0 {
		// No separator found — entire line is the key with empty value
		return content, ""
	}

	key := content[:pos]
	// Trim trailing space from key (space before separator)
	key = strings.TrimRight(key, " \t")

	rest := content[pos+len(sep):]
	// Trim all leading and trailing whitespace from value
	rest = strings.TrimSpace(rest)

	return key, rest
}

// findUnescapedSep finds the first occurrence of sep in text that is not
// immediately preceded by a backslash.
//
// Simple escape rule: a backslash directly before the separator escapes it.
// No pair processing — \\ before separator means the second \ escapes the
// separator, regardless of how many backslashes precede it.
func findUnescapedSep(text, sep string) int {
	sepLen := len(sep)
	for i := 0; i <= len(text)-sepLen; i++ {
		if text[i:i+sepLen] == sep {
			if i > 0 && text[i-1] == '\\' {
				// Backslash immediately before separator = escaped
				continue
			}
			return i
		}
	}
	return -1
}

// tryParseHeader attempts to parse a header line like "#= kvl 1.0 - option=val".
// Returns the config and true if successful, or zero Config and false if not a header.
func tryParseHeader(line string) (Config, bool) {
	line = strings.TrimSpace(line)
	if len(line) < 2 || line[0] != '#' {
		return Config{}, false
	}

	// Find "kvl" in the line to determine where the separator ends
	kvlIdx := strings.Index(line, " kvl")
	if kvlIdx < 1 {
		return Config{}, false
	}

	sep := line[1:kvlIdx]
	if sep == "" {
		return Config{}, false
	}

	rest := line[kvlIdx+4:] // skip " kvl"
	rest = strings.TrimLeft(rest, " \t")

	cfg := Config{
		Separator: sep,
		Options:   map[string]string{},
	}

	// Parse remaining tokens: version, optional list-markers, optional options
	tokens := strings.Fields(rest)
	tokenIdx := 0

	// Version
	if tokenIdx < len(tokens) {
		cfg.Version = tokens[tokenIdx]
		tokenIdx++
	}

	// Remaining tokens: list markers or options
	for tokenIdx < len(tokens) {
		tok := tokens[tokenIdx]
		if strings.Contains(tok, "=") {
			// Option: key=value
			parts := strings.SplitN(tok, "=", 2)
			cfg.Options[parts[0]] = parts[1]
		} else if tok == "strict" {
			// Bare "strict" flag
			cfg.Strict = true
		} else {
			// Could be list markers (characters like -, +, *)
			// Heuristic: if it doesn't contain = and is short, treat as markers
			cfg.ListMarkers = tok
		}
		tokenIdx++
	}

	// Check for strict=true in options
	if v, ok := cfg.Options["strict"]; ok && v == "true" {
		cfg.Strict = true
	}

	return cfg, true
}

// unescapeNode walks the tree and replaces \{sep} with {sep} in all keys.
func unescapeNode(n *node, sep string) {
	escaped := `\` + sep
	for i := range n.entries {
		if strings.Contains(n.entries[i].key, escaped) {
			n.entries[i].key = unescapeSep(n.entries[i].key, sep)
		}
		// Update index after key change
		unescapeNode(n.entries[i].value, sep)
	}
	// Rebuild index
	n.index = make(map[string]int, len(n.entries))
	for i, e := range n.entries {
		n.index[e.key] = i
	}
}

// unescapeSep replaces escaped separators in text.
// Handles multiple backslashes: \\ before sep means literal backslash + separator.
func unescapeSep(text, sep string) string {
	escaped := `\` + sep
	if !strings.Contains(text, escaped) {
		return text
	}

	var result strings.Builder
	i := 0
	for i < len(text) {
		if i < len(text)-len(sep) && text[i] == '\\' && text[i+1:i+1+len(sep)] == sep {
			result.WriteString(sep)
			i += 1 + len(sep)
		} else {
			result.WriteByte(text[i])
			i++
		}
	}
	return result.String()
}

// trimMultiline trims a multiline string value for loads()-level output.
// If the string starts with \n (block multiline from empty-value key),
// strip the leading newline, find min indent of non-blank lines, and dedent all.
// If it does NOT start with \n (valued-key continuation), the first line is
// inline; compute min indent from lines 2+ and dedent only those lines.
func trimMultiline(value string) string {
	if !strings.Contains(value, "\n") {
		return value
	}

	if strings.HasPrefix(value, "\n") {
		// Block multiline: strip leading newline, then dedent all lines
		body := value[1:]
		bodyLines := strings.Split(body, "\n")

		// Find min indent among non-blank lines
		minIndent := math.MaxInt
		for _, line := range bodyLines {
			if isBlank(line) {
				continue
			}
			indent := countLeadingSpaces(line)
			if indent < minIndent {
				minIndent = indent
			}
		}

		if minIndent == math.MaxInt || minIndent == 0 {
			return body
		}

		// Dedent all lines
		var b strings.Builder
		for i, line := range bodyLines {
			if i > 0 {
				b.WriteByte('\n')
			}
			if isBlank(line) {
				b.WriteString(line)
			} else if len(line) >= minIndent {
				b.WriteString(line[minIndent:])
			} else {
				b.WriteString(line)
			}
		}
		return b.String()
	}

	// Valued-key continuation: first line is inline, dedent lines 2+
	allLines := strings.Split(value, "\n")
	if len(allLines) <= 1 {
		return value
	}

	// Find min indent from lines 2+ (non-blank)
	minIndent := math.MaxInt
	for _, line := range allLines[1:] {
		if isBlank(line) {
			continue
		}
		indent := countLeadingSpaces(line)
		if indent < minIndent {
			minIndent = indent
		}
	}

	if minIndent == math.MaxInt || minIndent == 0 {
		return value
	}

	// Build result: first line unchanged, dedent lines 2+
	var b strings.Builder
	b.WriteString(allLines[0])
	for _, line := range allLines[1:] {
		b.WriteByte('\n')
		if isBlank(line) {
			b.WriteString(line)
		} else if len(line) >= minIndent {
			b.WriteString(line[minIndent:])
		} else {
			b.WriteString(line)
		}
	}
	return b.String()
}

// countLeadingSpaces counts the number of leading space/tab characters
// in a string (spaces count as 1, tabs count as 1 for trimming purposes).
func countLeadingSpaces(s string) int {
	count := 0
	for _, ch := range s {
		if ch == ' ' || ch == '\t' {
			count++
		} else {
			break
		}
	}
	return count
}

// trimMultilineValues walks a map[string]any and applies trimMultiline
// to all string values, recursing into nested maps.
func trimMultilineValues(m map[string]any) {
	for k, v := range m {
		switch val := v.(type) {
		case string:
			m[k] = trimMultiline(val)
		case map[string]any:
			trimMultilineValues(val)
		case []string:
			for i, s := range val {
				val[i] = trimMultiline(s)
			}
		case []any:
			for i, item := range val {
				if s, ok := item.(string); ok {
					val[i] = trimMultiline(s)
				} else if sub, ok := item.(map[string]any); ok {
					trimMultilineValues(sub)
				}
			}
		}
	}
}
