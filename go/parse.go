package kvl

import (
	"strings"
)

// parse is the main entry point for parsing KVL text.
// Returns the root node and the config parsed from any header.
func parse(text string) (*node, Config, error) {
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
	_, err := buildTree(root, lines, startLine, -1, cfg)
	if err != nil {
		return nil, cfg, err
	}

	unescapeNode(root, cfg.Separator)
	return root, cfg, nil
}

// buildTree recursively builds the node tree from lines.
// parentIndent is the indentation level of the parent block (-1 for root).
// Returns the next line index to process.
func buildTree(parent *node, lines []string, startLine, parentIndent int, cfg Config) (int, error) {
	blockIndent := -1
	i := startLine

	for i < len(lines) {
		line := lines[i]

		// Skip blank lines
		if isBlank(line) {
			i++
			continue
		}

		indent, err := measureIndent(line)
		if err != nil {
			return 0, &ParseError{Line: i + 1, Message: err.Error()}
		}

		// If indent <= parentIndent, we're done with this block
		if indent <= parentIndent {
			return i, nil
		}

		// Set block indent from first non-blank line
		if blockIndent == -1 {
			blockIndent = indent
		} else if indent != blockIndent {
			// This line is more indented than the block — could be a child.
			// But if it's less indented than blockIndent, that's an error or end of block.
			if indent < blockIndent {
				return i, nil
			}
			// More indented than blockIndent but we're not inside a nested object.
			// This shouldn't happen at this level — it's a consistency error.
			return 0, &ParseError{Line: i + 1, Message: "inconsistent indentation"}
		}

		content := strings.TrimLeft(line, " \t")
		key, value := parseLine(content, cfg)

		if key == "" && value != "" {
			// Empty key with value (from list markers or "= value"):
			// Add value directly as categorical entry in parent
			leaf := newNode()
			leaf.addEntry(value, newNode())
			parent.mergeFrom(leaf)
			i++
		} else if value == "" {
			// Empty value: could have indented children
			child := newNode()
			nextLine, err := buildTree(child, lines, i+1, blockIndent, cfg)
			if err != nil {
				return 0, err
			}

			if child.isEmpty() {
				// No children found — treat as empty string value
				leaf := newNode()
				leaf.addEntry("", newNode())
				parent.addEntry(key, leaf)
			} else {
				parent.addEntry(key, child)
			}
			i = nextLine
		} else {
			// Leaf: value is a string, create categorical entry {value: {}}
			leaf := newNode()
			leaf.addEntry(value, newNode())
			parent.addEntry(key, leaf)
			i++
		}
	}

	return i, nil
}

// measureIndent counts the effective indentation of a line.
// Tabs count as 4 spaces each.
func measureIndent(line string) (int, error) {
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
	// Trim leading space from value (space after separator)
	if len(rest) > 0 && (rest[0] == ' ' || rest[0] == '\t') {
		rest = rest[1:]
	}

	return key, rest
}

// findUnescapedSep finds the first occurrence of sep in text that is not
// preceded by an odd number of backslashes.
func findUnescapedSep(text, sep string) int {
	sepLen := len(sep)
	for i := 0; i <= len(text)-sepLen; i++ {
		if text[i:i+sepLen] == sep {
			// Count preceding backslashes
			bs := 0
			for j := i - 1; j >= 0 && text[j] == '\\'; j-- {
				bs++
			}
			if bs%2 == 0 {
				return i
			}
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
		} else {
			// Could be list markers (characters like -, +, *)
			// Heuristic: if it doesn't contain = and is short, treat as markers
			cfg.ListMarkers = tok
		}
		tokenIdx++
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
