//! The KVL data model.
//!
//! `Value` covers both the raw categorical model (nested string-keyed maps,
//! where every leaf is an empty map) and the compacted user-facing model
//! (strings, lists, and maps). Insertion order of map keys is preserved.

use indexmap::IndexMap;
use serde::de::{MapAccess, SeqAccess, Visitor};
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;

/// Ordered string-keyed map used throughout KVL.
pub type Map = IndexMap<String, Value>;

/// A KVL value: a string leaf, a list, or an ordered map.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    List(Vec<Value>),
    Map(Map),
}

impl Value {
    /// An empty map, the identity element of the merge monoid.
    pub fn empty() -> Value {
        Value::Map(Map::new())
    }

    pub fn is_empty_map(&self) -> bool {
        matches!(self, Value::Map(m) if m.is_empty())
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<&[Value]> {
        match self {
            Value::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_map(&self) -> Option<&Map> {
        match self {
            Value::Map(m) => Some(m),
            _ => None,
        }
    }

    /// Look up a key in a map value.
    pub fn get(&self, key: &str) -> Option<&Value> {
        match self {
            Value::Map(m) => m.get(key),
            _ => None,
        }
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Value {
        Value::String(s.to_string())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::String(s)
    }
}

impl Serialize for Value {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Value::String(s) => serializer.serialize_str(s),
            Value::List(items) => items.serialize(serializer),
            Value::Map(map) => {
                let mut m = serializer.serialize_map(Some(map.len()))?;
                for (k, v) in map {
                    m.serialize_entry(k, v)?;
                }
                m.end()
            }
        }
    }
}

/// Deserializing into `Value` maps foreign scalars onto KVL's string-based
/// model: booleans and numbers become their string representation and null
/// becomes the empty map (matching how the Python implementation expands
/// `None` before serialization).
impl<'de> Deserialize<'de> for Value {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Value, D::Error> {
        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a KVL value (string, list, or map)")
            }

            fn visit_str<E>(self, v: &str) -> Result<Value, E> {
                Ok(Value::String(v.to_string()))
            }

            fn visit_string<E>(self, v: String) -> Result<Value, E> {
                Ok(Value::String(v))
            }

            fn visit_bool<E>(self, v: bool) -> Result<Value, E> {
                Ok(Value::String(if v { "true" } else { "false" }.to_string()))
            }

            fn visit_i64<E>(self, v: i64) -> Result<Value, E> {
                Ok(Value::String(v.to_string()))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Value, E> {
                Ok(Value::String(v.to_string()))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Value, E> {
                Ok(Value::String(v.to_string()))
            }

            fn visit_unit<E>(self) -> Result<Value, E> {
                Ok(Value::empty())
            }

            fn visit_none<E>(self) -> Result<Value, E> {
                Ok(Value::empty())
            }

            fn visit_some<D: Deserializer<'de>>(self, deserializer: D) -> Result<Value, D::Error> {
                Value::deserialize(deserializer)
            }

            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Value, A::Error> {
                let mut items = Vec::new();
                while let Some(item) = seq.next_element()? {
                    items.push(item);
                }
                Ok(Value::List(items))
            }

            fn visit_map<A: MapAccess<'de>>(self, mut access: A) -> Result<Value, A::Error> {
                let mut map = Map::new();
                while let Some((k, v)) = access.next_entry::<String, Value>()? {
                    map.insert(k, v);
                }
                Ok(Value::Map(map))
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
}
