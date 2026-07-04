//! Typed deserialization from KVL via serde.
//!
//! KVL is stringly-typed at the parse level; this deserializer performs type
//! inference at the serde boundary, so `port = 8080` can deserialize into a
//! `u16` field and `active = true` into a `bool`.

use crate::error::KvlError;
use crate::value::{Map, Value};
use serde::de::{
    self, DeserializeOwned, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess,
    VariantAccess, Visitor,
};
use serde::Deserializer;

/// Parse KVL text and deserialize it into a typed value.
///
/// ```
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Server {
///     host: String,
///     port: u16,
/// }
///
/// let config: Server = kvl::from_str("host = localhost\nport = 8080").unwrap();
/// assert_eq!(config.port, 8080);
/// ```
pub fn from_str<T: DeserializeOwned>(text: &str) -> Result<T, KvlError> {
    let value = crate::parser::loads(text)?;
    from_value(value)
}

/// Deserialize a typed value from an already-parsed (compacted) KVL value.
pub fn from_value<T: DeserializeOwned>(value: Value) -> Result<T, KvlError> {
    T::deserialize(value)
}

macro_rules! deserialize_parsed {
    ($method:ident, $ty:ty, $visit:ident) => {
        fn $method<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
            match &self {
                Value::String(s) => match s.trim().parse::<$ty>() {
                    Ok(v) => visitor.$visit(v),
                    Err(_) => Err(de::Error::custom(format!(
                        concat!("cannot parse {:?} as ", stringify!($ty)),
                        s
                    ))),
                },
                _ => Err(de::Error::custom(concat!(
                    "expected a string value for ",
                    stringify!($ty)
                ))),
            }
        }
    };
}

impl<'de> Deserializer<'de> for Value {
    type Error = KvlError;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        match self {
            Value::String(s) => visitor.visit_string(s),
            Value::List(items) => visit_list(items, visitor),
            Value::Map(map) => visit_map(map, visitor),
        }
    }

    fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        match &self {
            Value::String(s) => match s.trim().to_lowercase().as_str() {
                "true" => visitor.visit_bool(true),
                "false" => visitor.visit_bool(false),
                _ => Err(de::Error::custom(format!("cannot parse {s:?} as bool"))),
            },
            _ => Err(de::Error::custom("expected a string value for bool")),
        }
    }

    deserialize_parsed!(deserialize_i8, i8, visit_i8);
    deserialize_parsed!(deserialize_i16, i16, visit_i16);
    deserialize_parsed!(deserialize_i32, i32, visit_i32);
    deserialize_parsed!(deserialize_i64, i64, visit_i64);
    deserialize_parsed!(deserialize_i128, i128, visit_i128);
    deserialize_parsed!(deserialize_u8, u8, visit_u8);
    deserialize_parsed!(deserialize_u16, u16, visit_u16);
    deserialize_parsed!(deserialize_u32, u32, visit_u32);
    deserialize_parsed!(deserialize_u64, u64, visit_u64);
    deserialize_parsed!(deserialize_u128, u128, visit_u128);
    deserialize_parsed!(deserialize_f32, f32, visit_f32);
    deserialize_parsed!(deserialize_f64, f64, visit_f64);

    fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        match &self {
            Value::String(s) => {
                let mut chars = s.chars();
                match (chars.next(), chars.next()) {
                    (Some(c), None) => visitor.visit_char(c),
                    _ => Err(de::Error::custom(format!("cannot parse {s:?} as char"))),
                }
            }
            _ => Err(de::Error::custom("expected a string value for char")),
        }
    }

    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        self.deserialize_string(visitor)
    }

    fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        match self {
            Value::String(s) => visitor.visit_string(s),
            // An empty value parses to an empty map; expose it as "".
            Value::Map(m) if m.is_empty() => visitor.visit_str(""),
            _ => Err(de::Error::custom("expected a string value")),
        }
    }

    fn deserialize_bytes<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        match self {
            Value::String(s) => visitor.visit_byte_buf(s.into_bytes()),
            _ => Err(de::Error::custom("expected a string value for bytes")),
        }
    }

    fn deserialize_byte_buf<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        match &self {
            Value::Map(m) if m.is_empty() => visitor.visit_unit(),
            Value::String(s) if s.is_empty() => visitor.visit_unit(),
            _ => Err(de::Error::custom("expected an empty value for unit")),
        }
    }

    fn deserialize_unit_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, KvlError> {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, KvlError> {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        match self {
            Value::List(items) => visit_list(items, visitor),
            // Compaction collapses single-item lists to scalars; recover them.
            Value::String(_) => visit_list(vec![self], visitor),
            Value::Map(m) if m.is_empty() => visit_list(Vec::new(), visitor),
            Value::Map(m) => {
                let items: Vec<Value> = m
                    .into_iter()
                    .map(|(k, v)| {
                        let mut single = Map::new();
                        single.insert(k, v);
                        Value::Map(single)
                    })
                    .collect();
                visit_list(items, visitor)
            }
        }
    }

    fn deserialize_tuple<V: Visitor<'de>>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, KvlError> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, KvlError> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        match self {
            Value::Map(map) => visit_map(map, visitor),
            _ => Err(de::Error::custom("expected a map value")),
        }
    }

    fn deserialize_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, KvlError> {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, KvlError> {
        match self {
            Value::String(s) => visitor.visit_enum(s.into_deserializer()),
            Value::Map(m) if m.len() == 1 => {
                let (variant, value) = m.into_iter().next().unwrap();
                visitor.visit_enum(ValueEnumAccess { variant, value })
            }
            _ => Err(de::Error::custom(
                "expected a string or single-key map for enum",
            )),
        }
    }

    fn deserialize_identifier<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        self.deserialize_string(visitor)
    }

    fn deserialize_ignored_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, KvlError> {
        visitor.visit_unit()
    }
}

fn visit_list<'de, V: Visitor<'de>>(items: Vec<Value>, visitor: V) -> Result<V::Value, KvlError> {
    let mut deserializer = ListAccess {
        iter: items.into_iter(),
    };
    visitor.visit_seq(&mut deserializer)
}

fn visit_map<'de, V: Visitor<'de>>(map: Map, visitor: V) -> Result<V::Value, KvlError> {
    let mut deserializer = MapValueAccess {
        iter: map.into_iter(),
        value: None,
    };
    visitor.visit_map(&mut deserializer)
}

struct ListAccess {
    iter: std::vec::IntoIter<Value>,
}

impl<'de> SeqAccess<'de> for ListAccess {
    type Error = KvlError;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, KvlError> {
        match self.iter.next() {
            Some(value) => seed.deserialize(value).map(Some),
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.iter.len())
    }
}

struct MapValueAccess {
    iter: indexmap::map::IntoIter<String, Value>,
    value: Option<Value>,
}

impl<'de> MapAccess<'de> for MapValueAccess {
    type Error = KvlError;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, KvlError> {
        match self.iter.next() {
            Some((key, value)) => {
                self.value = Some(value);
                seed.deserialize(Value::String(key)).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(&mut self, seed: V) -> Result<V::Value, KvlError> {
        let value = self
            .value
            .take()
            .ok_or_else(|| de::Error::custom("value is missing"))?;
        seed.deserialize(value)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.iter.len())
    }
}

struct ValueEnumAccess {
    variant: String,
    value: Value,
}

impl<'de> EnumAccess<'de> for ValueEnumAccess {
    type Error = KvlError;
    type Variant = ValueVariantAccess;

    fn variant_seed<V: DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, ValueVariantAccess), KvlError> {
        let variant = seed.deserialize(Value::String(self.variant))?;
        Ok((variant, ValueVariantAccess { value: self.value }))
    }
}

struct ValueVariantAccess {
    value: Value,
}

impl<'de> VariantAccess<'de> for ValueVariantAccess {
    type Error = KvlError;

    fn unit_variant(self) -> Result<(), KvlError> {
        match &self.value {
            Value::Map(m) if m.is_empty() => Ok(()),
            Value::String(s) if s.is_empty() => Ok(()),
            _ => Err(de::Error::custom("expected an empty value for unit variant")),
        }
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value, KvlError> {
        seed.deserialize(self.value)
    }

    fn tuple_variant<V: Visitor<'de>>(self, _len: usize, visitor: V) -> Result<V::Value, KvlError> {
        self.value.deserialize_seq(visitor)
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, KvlError> {
        self.value.deserialize_map(visitor)
    }
}
