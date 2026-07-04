//! Structural transformations: categorical merge, compacting, and expansion.

use crate::config::KvlConfig;
use crate::value::{Map, Value};

/// Merge two categorical models recursively.
///
/// Maps merge key-by-key; a map wins over a non-map; two conflicting scalar
/// values are lifted into categorical form and merged, so
/// `port=8080 + port=8081` becomes `{port: {8080: {}, 8081: {}}}`.
pub fn merge(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Map(m1), Value::Map(m2)) => {
            let mut result = m1.clone();
            for (k, v) in m2 {
                match result.get(k) {
                    Some(existing) => {
                        let merged = merge(existing, v);
                        result.insert(k.clone(), merged);
                    }
                    None => {
                        result.insert(k.clone(), v.clone());
                    }
                }
            }
            Value::Map(result)
        }
        (Value::Map(_), _) => a.clone(),
        (_, Value::Map(_)) => b.clone(),
        _ => {
            let cat1 = to_categorical_scalar(a);
            let cat2 = to_categorical_scalar(b);
            merge(&cat1, &cat2)
        }
    }
}

fn to_categorical_scalar(v: &Value) -> Value {
    match v {
        Value::String(s) if s.is_empty() => Value::empty(),
        Value::String(s) => {
            let mut m = Map::new();
            m.insert(s.clone(), Value::empty());
            Value::Map(m)
        }
        Value::List(items) => {
            let mut result = Value::empty();
            for item in items {
                result = merge(&result, &to_categorical_scalar(item));
            }
            result
        }
        Value::Map(_) => v.clone(),
    }
}

/// Compact a categorical structure into the user-facing form:
///
/// 1. Singleton empty-key lifting: `{"": content}` becomes `content`
/// 2. Single empty child conversion: `{"value": {}}` becomes `"value"`
/// 3. Empty-value map flattening: `{"a": {}, "b": {}}` becomes `["a", "b"]`
/// 4. Section-header keys (starting with the separator) are never flattened
pub fn compact(data: &Value, config: &KvlConfig) -> Value {
    match data {
        Value::List(items) => Value::List(items.iter().map(|i| compact(i, config)).collect()),
        Value::String(_) => data.clone(),
        Value::Map(m) => {
            let compacted: Map = m
                .iter()
                .map(|(k, v)| (k.clone(), compact(v, config)))
                .collect();
            apply_compacting_rules(compacted, config)
        }
    }
}

fn apply_compacting_rules(data: Map, config: &KvlConfig) -> Value {
    if data.is_empty() {
        return Value::Map(data);
    }

    // Rule 1: singleton empty-key lifting
    if data.len() == 1 && data.contains_key("") {
        let empty_value = &data[""];
        if let Value::Map(ev) = empty_value {
            if !ev.is_empty() && ev.values().all(|v| matches!(v, Value::Map(_))) {
                return Value::List(
                    ev.iter()
                        .map(|(k, v)| {
                            let mut m = Map::new();
                            m.insert(k.clone(), v.clone());
                            Value::Map(m)
                        })
                        .collect(),
                );
            }
        }
        return empty_value.clone();
    }

    // Rule 2: single empty child converts to a string value
    if data.len() == 1 {
        let (key, value) = data.iter().next().unwrap();
        if value.is_empty_map() && !key.is_empty() {
            return Value::String(key.clone());
        }
    }

    // Rule 3: all-empty-value maps flatten to a list of keys
    if data.values().all(|v| v.is_empty_map()) {
        // Rule 4: keep maps whose keys look like section headers
        if data
            .keys()
            .any(|k| is_section_header(k, &config.separator))
        {
            return Value::Map(data);
        }
        return Value::List(data.keys().map(|k| Value::String(k.clone())).collect());
    }

    Value::Map(data)
}

fn is_section_header(value: &str, separator: &str) -> bool {
    let stripped = value.trim();
    !stripped.is_empty() && stripped.starts_with(separator)
}

/// Expand compacted data back to the categorical nested-map structure.
/// This is the inverse of `compact()`, used before categorical serialization.
pub fn expand(data: &Value) -> Value {
    match data {
        Value::List(items) => {
            let mut result = Map::new();
            for item in items {
                if let Value::String(s) = item {
                    result.insert(s.clone(), Value::empty());
                } else {
                    let expanded = expand(item);
                    let existing = result
                        .entry(String::new())
                        .or_insert_with(Value::empty)
                        .clone();
                    result.insert(String::new(), merge(&existing, &expanded));
                }
            }
            Value::Map(result)
        }
        Value::Map(m) => Value::Map(m.iter().map(|(k, v)| (k.clone(), expand(v))).collect()),
        Value::String(s) => {
            if s.contains('\n') {
                data.clone()
            } else {
                let mut m = Map::new();
                m.insert(s.clone(), Value::empty());
                Value::Map(m)
            }
        }
    }
}
