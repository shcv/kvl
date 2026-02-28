package kvl

import (
	"sort"
	"strings"
)

// serialize converts a map[string]any to KVL text with the given separator.
func serialize(data map[string]any, sep string) string {
	var b strings.Builder
	writeMap(&b, data, sep, 0)
	return b.String()
}

// writeMap writes a map's entries at the given indent level.
func writeMap(b *strings.Builder, data map[string]any, sep string, indent int) {
	keys := sortedKeys(data)
	for _, k := range keys {
		v := data[k]
		writeEntry(b, k, v, sep, indent)
	}
}

// writeEntry writes a single key-value entry.
func writeEntry(b *strings.Builder, key string, value any, sep string, indent int) {
	prefix := strings.Repeat("    ", indent)
	escapedKey := escapeSep(key, sep)

	switch v := value.(type) {
	case map[string]any:
		if len(v) == 0 {
			// Empty map — write as key with empty value
			b.WriteString(prefix)
			b.WriteString(escapedKey)
			b.WriteString(" ")
			b.WriteString(sep)
			b.WriteString("\n")
			return
		}

		if isCategorical(v) {
			// Categorical: write as repeated keys
			catKeys := sortedKeys(v)
			for _, ck := range catKeys {
				b.WriteString(prefix)
				b.WriteString(escapedKey)
				b.WriteString(" ")
				b.WriteString(sep)
				b.WriteString(" ")
				b.WriteString(escapeSep(ck, sep))
				b.WriteString("\n")
			}
			return
		}

		// Nested object
		b.WriteString(prefix)
		b.WriteString(escapedKey)
		b.WriteString(" ")
		b.WriteString(sep)
		b.WriteString("\n")
		writeMap(b, v, sep, indent+1)

	case string:
		b.WriteString(prefix)
		b.WriteString(escapedKey)
		b.WriteString(" ")
		b.WriteString(sep)
		b.WriteString(" ")
		b.WriteString(escapeSep(v, sep))
		b.WriteString("\n")

	case []string:
		// List: write as repeated keys
		for _, s := range v {
			b.WriteString(prefix)
			b.WriteString(escapedKey)
			b.WriteString(" ")
			b.WriteString(sep)
			b.WriteString(" ")
			b.WriteString(escapeSep(s, sep))
			b.WriteString("\n")
		}

	case []any:
		for _, item := range v {
			if s, ok := item.(string); ok {
				b.WriteString(prefix)
				b.WriteString(escapedKey)
				b.WriteString(" ")
				b.WriteString(sep)
				b.WriteString(" ")
				b.WriteString(escapeSep(s, sep))
				b.WriteString("\n")
			}
		}
	}
}

// escapeSep escapes separator characters in text with backslash.
func escapeSep(text, sep string) string {
	return strings.ReplaceAll(text, sep, `\`+sep)
}

// sortedKeys returns the keys of a map in sorted order.
func sortedKeys(m map[string]any) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
