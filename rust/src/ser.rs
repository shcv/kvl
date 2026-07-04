//! Typed serialization to KVL via serde.
//!
//! Scalars are rendered as strings (KVL's native value type), sequences become
//! KVL lists, and maps/structs become nested KVL maps.

use crate::config::KvlConfig;
use crate::error::KvlError;
use crate::serializer::{dumps_with, DumpOptions};
use crate::value::{Map, Value};
use serde::ser::{self, Serialize};

/// Serialize a typed value to KVL text with the default configuration.
///
/// ```
/// use serde::Serialize;
///
/// #[derive(Serialize)]
/// struct Server {
///     host: String,
///     port: u16,
/// }
///
/// let text = kvl::to_string(&Server { host: "localhost".into(), port: 8080 }).unwrap();
/// assert_eq!(text, "host = localhost\nport = 8080\n");
/// ```
pub fn to_string<T: Serialize>(value: &T) -> Result<String, KvlError> {
    let options = DumpOptions {
        public_format: true,
        ..DumpOptions::default()
    };
    to_string_with(value, &KvlConfig::default(), &options)
}

/// Serialize a typed value to KVL text with explicit configuration.
pub fn to_string_with<T: Serialize>(
    value: &T,
    config: &KvlConfig,
    options: &DumpOptions,
) -> Result<String, KvlError> {
    let v = to_value(value)?;
    dumps_with(&v, config, options)
}

/// Convert any serializable value into a KVL `Value`.
pub fn to_value<T: Serialize>(value: &T) -> Result<Value, KvlError> {
    value.serialize(ValueSerializer)
}

struct ValueSerializer;

macro_rules! serialize_stringified {
    ($method:ident, $ty:ty) => {
        fn $method(self, v: $ty) -> Result<Value, KvlError> {
            Ok(Value::String(v.to_string()))
        }
    };
}

impl ser::Serializer for ValueSerializer {
    type Ok = Value;
    type Error = KvlError;
    type SerializeSeq = SeqBuilder;
    type SerializeTuple = SeqBuilder;
    type SerializeTupleStruct = SeqBuilder;
    type SerializeTupleVariant = VariantSeqBuilder;
    type SerializeMap = MapBuilder;
    type SerializeStruct = MapBuilder;
    type SerializeStructVariant = VariantMapBuilder;

    fn serialize_bool(self, v: bool) -> Result<Value, KvlError> {
        Ok(Value::String(if v { "true" } else { "false" }.to_string()))
    }

    serialize_stringified!(serialize_i8, i8);
    serialize_stringified!(serialize_i16, i16);
    serialize_stringified!(serialize_i32, i32);
    serialize_stringified!(serialize_i64, i64);
    serialize_stringified!(serialize_i128, i128);
    serialize_stringified!(serialize_u8, u8);
    serialize_stringified!(serialize_u16, u16);
    serialize_stringified!(serialize_u32, u32);
    serialize_stringified!(serialize_u64, u64);
    serialize_stringified!(serialize_u128, u128);
    serialize_stringified!(serialize_f32, f32);
    serialize_stringified!(serialize_f64, f64);
    serialize_stringified!(serialize_char, char);

    fn serialize_str(self, v: &str) -> Result<Value, KvlError> {
        Ok(Value::String(v.to_string()))
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Value, KvlError> {
        Err(ser::Error::custom("KVL does not support raw bytes"))
    }

    fn serialize_none(self) -> Result<Value, KvlError> {
        Ok(Value::empty())
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Value, KvlError> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Value, KvlError> {
        Ok(Value::empty())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value, KvlError> {
        Ok(Value::empty())
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _index: u32,
        variant: &'static str,
    ) -> Result<Value, KvlError> {
        Ok(Value::String(variant.to_string()))
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Value, KvlError> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        _index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Value, KvlError> {
        let mut map = Map::new();
        map.insert(variant.to_string(), value.serialize(ValueSerializer)?);
        Ok(Value::Map(map))
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<SeqBuilder, KvlError> {
        Ok(SeqBuilder {
            items: Vec::with_capacity(len.unwrap_or(0)),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<SeqBuilder, KvlError> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<SeqBuilder, KvlError> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<VariantSeqBuilder, KvlError> {
        Ok(VariantSeqBuilder {
            variant,
            items: Vec::with_capacity(len),
        })
    }

    fn serialize_map(self, len: Option<usize>) -> Result<MapBuilder, KvlError> {
        Ok(MapBuilder {
            map: Map::with_capacity(len.unwrap_or(0)),
            key: None,
        })
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<MapBuilder, KvlError> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<VariantMapBuilder, KvlError> {
        Ok(VariantMapBuilder {
            variant,
            map: Map::with_capacity(len),
        })
    }
}

struct SeqBuilder {
    items: Vec<Value>,
}

impl ser::SerializeSeq for SeqBuilder {
    type Ok = Value;
    type Error = KvlError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), KvlError> {
        self.items.push(value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value, KvlError> {
        Ok(Value::List(self.items))
    }
}

impl ser::SerializeTuple for SeqBuilder {
    type Ok = Value;
    type Error = KvlError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), KvlError> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Value, KvlError> {
        ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleStruct for SeqBuilder {
    type Ok = Value;
    type Error = KvlError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), KvlError> {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Value, KvlError> {
        ser::SerializeSeq::end(self)
    }
}

struct VariantSeqBuilder {
    variant: &'static str,
    items: Vec<Value>,
}

impl ser::SerializeTupleVariant for VariantSeqBuilder {
    type Ok = Value;
    type Error = KvlError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), KvlError> {
        self.items.push(value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value, KvlError> {
        let mut map = Map::new();
        map.insert(self.variant.to_string(), Value::List(self.items));
        Ok(Value::Map(map))
    }
}

struct MapBuilder {
    map: Map,
    key: Option<String>,
}

impl ser::SerializeMap for MapBuilder {
    type Ok = Value;
    type Error = KvlError;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), KvlError> {
        match key.serialize(ValueSerializer)? {
            Value::String(s) => {
                self.key = Some(s);
                Ok(())
            }
            _ => Err(ser::Error::custom("KVL map keys must be strings")),
        }
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), KvlError> {
        let key = self
            .key
            .take()
            .ok_or_else(|| ser::Error::custom("serialize_value called before serialize_key"))?;
        self.map.insert(key, value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value, KvlError> {
        Ok(Value::Map(self.map))
    }
}

impl ser::SerializeStruct for MapBuilder {
    type Ok = Value;
    type Error = KvlError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), KvlError> {
        self.map
            .insert(key.to_string(), value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value, KvlError> {
        Ok(Value::Map(self.map))
    }
}

struct VariantMapBuilder {
    variant: &'static str,
    map: Map,
}

impl ser::SerializeStructVariant for VariantMapBuilder {
    type Ok = Value;
    type Error = KvlError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), KvlError> {
        self.map
            .insert(key.to_string(), value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Value, KvlError> {
        let mut outer = Map::new();
        outer.insert(self.variant.to_string(), Value::Map(self.map));
        Ok(Value::Map(outer))
    }
}
