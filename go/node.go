package kvl

// node is an ordered tree of key-value entries.
// It preserves insertion order for list compaction while supporting
// categorical merge semantics for repeated keys.
type node struct {
	entries []entry
	index   map[string]int // key -> position in entries
	isList  bool           // true if this node represents a list of items
}

type entry struct {
	key   string
	value *node
}

func newNode() *node {
	return &node{
		index: map[string]int{},
	}
}

// addEntry adds a key-child pair, merging if the key already exists.
func (n *node) addEntry(key string, child *node) {
	if idx, ok := n.index[key]; ok {
		n.entries[idx].value.mergeFrom(child)
	} else {
		n.index[key] = len(n.entries)
		n.entries = append(n.entries, entry{key: key, value: child})
	}
}

// addListEntry adds an entry to a list node. Each entry is kept separate
// (not merged by key) to preserve list semantics.
func (n *node) addListEntry(key string, child *node) {
	n.isList = true
	// For list items, always append — don't merge by key
	n.entries = append(n.entries, entry{key: key, value: child})
}

// mergeFrom merges all entries from other into n.
func (n *node) mergeFrom(other *node) {
	if other.isList {
		// If other is a list, add entries as list items
		n.isList = true
		for _, e := range other.entries {
			n.addListEntry(e.key, e.value)
		}
	} else {
		for _, e := range other.entries {
			n.addEntry(e.key, e.value)
		}
	}
}

// isEmpty returns true if the node has no entries.
func (n *node) isEmpty() bool {
	return len(n.entries) == 0
}

// allChildrenEmpty returns true if every child node has no entries.
func (n *node) allChildrenEmpty() bool {
	for _, e := range n.entries {
		if !e.value.isEmpty() {
			return false
		}
	}
	return true
}

// toMap converts the node tree to a categorical map[string]any.
// Every leaf becomes an empty map[string]any{}.
// List nodes produce []any with each entry as a separate map.
func (n *node) toMap() map[string]any {
	result := make(map[string]any, len(n.entries))
	for _, e := range n.entries {
		if e.value.isEmpty() {
			result[e.key] = map[string]any{}
		} else {
			result[e.key] = e.value.toCategorical()
		}
	}
	return result
}

// toCategorical converts a node to its categorical representation.
// For list nodes, returns []any. For regular nodes, returns map[string]any.
func (n *node) toCategorical() any {
	if n.isEmpty() {
		return map[string]any{}
	}

	if n.isList {
		result := make([]any, len(n.entries))
		for i, e := range n.entries {
			if e.key == "" {
				if e.value.isEmpty() {
					result[i] = map[string]any{}
				} else {
					result[i] = e.value.toCategorical()
				}
			} else if e.value.isEmpty() {
				result[i] = map[string]any{e.key: map[string]any{}}
			} else {
				result[i] = map[string]any{e.key: e.value.toCategorical()}
			}
		}
		return result
	}

	return n.toMap()
}

// toCompacted converts the node tree to a user-friendly format.
//
// Compaction rules (applied recursively to child values):
//  1. Empty node -> map[string]any{}
//  2. All children empty, 1 child -> string (the key)
//  3. All children empty, N>1 children -> []string (preserving insertion order)
//  4. Otherwise -> map[string]any, recurse into each child
func (n *node) toCompacted() any {
	if n.isEmpty() {
		return map[string]any{}
	}

	if n.isList {
		allScalars := true
		for _, e := range n.entries {
			if e.key == "" || !e.value.isEmpty() {
				allScalars = false
				break
			}
		}
		if allScalars {
			result := make([]string, len(n.entries))
			for i, e := range n.entries {
				result[i] = e.key
			}
			return result
		}

		result := make([]any, len(n.entries))
		for i, e := range n.entries {
			if e.key == "" {
				result[i] = e.value.toCompacted()
			} else if e.value.isEmpty() {
				result[i] = e.key
			} else {
				result[i] = map[string]any{e.key: e.value.toCompacted()}
			}
		}
		return result
	}

	if n.allChildrenEmpty() {
		if len(n.entries) == 1 {
			return n.entries[0].key
		}
		keys := make([]string, len(n.entries))
		for i, e := range n.entries {
			keys[i] = e.key
		}
		return keys
	}

	result := make(map[string]any, len(n.entries))
	for _, e := range n.entries {
		result[e.key] = e.value.toCompacted()
	}
	return result
}

// toCompactedMap is like toCompacted but always returns a map[string]any.
// Used for the root node to ensure the top-level result is always a map.
func (n *node) toCompactedMap() map[string]any {
	result := make(map[string]any, len(n.entries))
	for _, e := range n.entries {
		result[e.key] = e.value.toCompacted()
	}
	return result
}
