package kvl

import "sort"

// compactMap converts a categorical map to compacted form.
// Maps where all values are empty maps become sorted []string or plain string.
func compactMap(data map[string]any) map[string]any {
	result := make(map[string]any, len(data))
	for k, v := range data {
		result[k] = compactValue(v)
	}
	return result
}

// compactValue compacts a single value.
func compactValue(v any) any {
	m, ok := v.(map[string]any)
	if !ok {
		return v
	}

	if len(m) == 0 {
		return m
	}

	if isCategorical(m) {
		if len(m) == 1 {
			for k := range m {
				return k
			}
		}
		keys := make([]string, 0, len(m))
		for k := range m {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		return keys
	}

	return compactMap(m)
}

// isCategorical returns true if all values in the map are empty maps.
func isCategorical(m map[string]any) bool {
	for _, v := range m {
		sub, ok := v.(map[string]any)
		if !ok || len(sub) != 0 {
			return false
		}
	}
	return true
}

// expandMap converts a compacted map back to categorical form.
func expandMap(data map[string]any) map[string]any {
	result := make(map[string]any, len(data))
	for k, v := range data {
		result[k] = expandValue(v)
	}
	return result
}

// expandValue expands a single value to categorical form.
func expandValue(v any) any {
	switch val := v.(type) {
	case string:
		return map[string]any{val: map[string]any{}}
	case []string:
		m := make(map[string]any, len(val))
		for _, s := range val {
			m[s] = map[string]any{}
		}
		return m
	case []any:
		m := make(map[string]any, len(val))
		for _, item := range val {
			if s, ok := item.(string); ok {
				m[s] = map[string]any{}
			}
		}
		return m
	case map[string]any:
		return expandMap(val)
	default:
		return v
	}
}
