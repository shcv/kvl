package kvl

// mergeMap performs an associative merge of two categorical maps.
// Both maps and non-map values are handled:
//   - Both maps: recursive merge
//   - Otherwise: later value overrides (shouldn't happen in proper categorical data)
func mergeMap(a, b map[string]any) map[string]any {
	result := make(map[string]any, len(a)+len(b))

	for k, v := range a {
		result[k] = v
	}

	for k, v := range b {
		if existing, ok := result[k]; ok {
			result[k] = mergeValues(existing, v)
		} else {
			result[k] = v
		}
	}

	return result
}

// mergeValues merges two values.
func mergeValues(a, b any) any {
	aMap, aOk := a.(map[string]any)
	bMap, bOk := b.(map[string]any)

	if aOk && bOk {
		return mergeMap(aMap, bMap)
	}

	// Non-map values: b overrides a
	return b
}
