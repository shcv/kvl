package kvl

import (
	"fmt"
	"sort"
	"strings"
)

// serialize converts a map[string]any to KVL text using the given config.
func serialize(data map[string]any, cfg Config) (string, error) {
	var b strings.Builder
	s := serializer{
		config:     cfg,
		indent:     "    ",
		listMarker: determineListMarker(cfg),
	}
	if err := s.writeMap(&b, data, 0); err != nil {
		return "", err
	}
	return b.String(), nil
}

type serializer struct {
	config     Config
	indent     string
	listMarker string
}

func determineListMarker(cfg Config) string {
	if cfg.ListMarkers == "" {
		return ""
	}
	return string([]rune(cfg.ListMarkers)[0])
}

func (s serializer) writeMap(b *strings.Builder, data map[string]any, level int) error {
	for _, key := range sortedKeys(data) {
		if err := s.writeEntry(b, key, data[key], level); err != nil {
			return err
		}
	}
	return nil
}

func (s serializer) writeEntry(b *strings.Builder, key string, value any, level int) error {
	indent := strings.Repeat(s.indent, level)
	escapedKey := escapeSep(key, s.config.Separator)

	switch v := value.(type) {
	case map[string]any:
		b.WriteString(indent)
		b.WriteString(escapedKey)
		b.WriteString(formatSep(s.config, true))
		b.WriteString("\n")
		if len(v) > 0 {
			return s.writeMap(b, v, level+1)
		}
		return nil
	case []string:
		return s.writeListEntry(b, escapedKey, toAnySlice(v), level)
	case []any:
		return s.writeListEntry(b, escapedKey, v, level)
	case nil:
		b.WriteString(indent)
		b.WriteString(escapedKey)
		b.WriteString(formatSep(s.config, false))
		b.WriteString("null\n")
		return nil
	case string:
		return s.writeScalarEntry(b, escapedKey, v, level)
	case bool:
		return s.writeScalarEntry(b, escapedKey, fmt.Sprint(v), level)
	case float64, float32, int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64:
		return s.writeScalarEntry(b, escapedKey, fmt.Sprint(v), level)
	default:
		return fmt.Errorf("unsupported value type: %T", value)
	}
}

func (s serializer) writeListEntry(b *strings.Builder, escapedKey string, items []any, level int) error {
	indent := strings.Repeat(s.indent, level)
	if s.listMarker == "" {
		for _, item := range items {
			if isComplexListItem(item) {
				return fmt.Errorf("complex list items require configured list markers")
			}
			text, err := scalarText(item)
			if err != nil {
				return err
			}
			b.WriteString(indent)
			b.WriteString(escapedKey)
			b.WriteString(formatSep(s.config, false))
			b.WriteString(escapeSep(text, s.config.Separator))
			b.WriteString("\n")
		}
		return nil
	}

	b.WriteString(indent)
	b.WriteString(escapedKey)
	b.WriteString(formatSep(s.config, true))
	b.WriteString("\n")
	return s.writeList(b, items, level+1)
}

func (s serializer) writeList(b *strings.Builder, items []any, level int) error {
	indent := strings.Repeat(s.indent, level)
	for _, item := range items {
		switch v := item.(type) {
		case string:
			b.WriteString(indent)
			b.WriteString(s.listMarker)
			b.WriteString(" ")
			b.WriteString(escapeSep(v, s.config.Separator))
			b.WriteString("\n")
		case nil:
			b.WriteString(indent)
			b.WriteString(s.listMarker)
			b.WriteString(" null\n")
		case bool:
			b.WriteString(indent)
			b.WriteString(s.listMarker)
			b.WriteString(" ")
			b.WriteString(fmt.Sprint(v))
			b.WriteString("\n")
		case float64, float32, int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64:
			b.WriteString(indent)
			b.WriteString(s.listMarker)
			b.WriteString(" ")
			b.WriteString(fmt.Sprint(v))
			b.WriteString("\n")
		case []string:
			b.WriteString(indent)
			b.WriteString(s.listMarker)
			b.WriteString("\n")
			if err := s.writeList(b, toAnySlice(v), level+1); err != nil {
				return err
			}
		case []any:
			b.WriteString(indent)
			b.WriteString(s.listMarker)
			b.WriteString("\n")
			if err := s.writeList(b, v, level+1); err != nil {
				return err
			}
		case map[string]any:
			if len(v) == 0 {
				b.WriteString(indent)
				b.WriteString(s.listMarker)
				b.WriteString("\n")
				continue
			}
			if len(v) == 1 {
				k := firstKey(v)
				if isSimpleScalar(v[k]) {
					text, err := scalarText(v[k])
					if err != nil {
						return err
					}
					b.WriteString(indent)
					b.WriteString(s.listMarker)
					b.WriteString(" ")
					b.WriteString(escapeSep(k, s.config.Separator))
					b.WriteString(formatSep(s.config, false))
					b.WriteString(escapeSep(text, s.config.Separator))
					b.WriteString("\n")
					continue
				}
			}
			b.WriteString(indent)
			b.WriteString(s.listMarker)
			b.WriteString("\n")
			if err := s.writeMap(b, v, level+1); err != nil {
				return err
			}
		default:
			return fmt.Errorf("unsupported list item type: %T", item)
		}
	}
	return nil
}

func (s serializer) writeScalarEntry(b *strings.Builder, escapedKey, value string, level int) error {
	indent := strings.Repeat(s.indent, level)
	b.WriteString(indent)
	b.WriteString(escapedKey)
	if value == "" {
		b.WriteString(formatSep(s.config, true))
		b.WriteString("\n")
		return nil
	}
	if strings.Contains(value, "\n") {
		lines := strings.Split(value, "\n")
		b.WriteString(formatSep(s.config, true))
		b.WriteString("\n")
		childIndent := strings.Repeat(s.indent, level+1)
		for _, line := range lines {
			b.WriteString(childIndent)
			b.WriteString(line)
			b.WriteString("\n")
		}
		return nil
	}
	b.WriteString(formatSep(s.config, false))
	b.WriteString(escapeSep(value, s.config.Separator))
	b.WriteString("\n")
	return nil
}

func formatSep(cfg Config, forEmpty bool) string {
	before := " "
	if cfg.Separator == ":" {
		before = ""
	}
	after := ""
	if !forEmpty {
		after = " "
	}
	return before + cfg.Separator + after
}

func scalarText(value any) (string, error) {
	switch v := value.(type) {
	case nil:
		return "null", nil
	case string:
		return v, nil
	case bool:
		return fmt.Sprint(v), nil
	case float64, float32, int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64:
		return fmt.Sprint(v), nil
	default:
		return "", fmt.Errorf("unsupported scalar list item type: %T", value)
	}
}

func isComplexListItem(value any) bool {
	switch value.(type) {
	case []any, []string, map[string]any:
		return true
	default:
		return false
	}
}

func isSimpleScalar(value any) bool {
	switch value.(type) {
	case nil, string, bool, float64, float32, int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64:
		return true
	default:
		return false
	}
}

func firstKey(m map[string]any) string {
	for k := range m {
		return k
	}
	return ""
}

func toAnySlice(items []string) []any {
	result := make([]any, len(items))
	for i, item := range items {
		result[i] = item
	}
	return result
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
