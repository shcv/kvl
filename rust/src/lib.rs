//! # KVL — Key-Value Language
//!
//! A minimalist configuration format with three core principles:
//!
//! 1. **Simplicity**: clean syntax with minimal boilerplate
//! 2. **Composability**: mathematical merge operations (monoid properties)
//! 3. **Mathematical soundness**: associative merging where `(A + B) + C = A + (B + C)`
//!
//! Based on the original CCL by Dmitrii Kovanikov (chshersh):
//! <https://github.com/chshersh/ccl>
//!
//! ## Quick start
//!
//! ```
//! // Untyped: parse to the dynamic Value model
//! let value = kvl::loads("name = test\nport = 8080").unwrap();
//! assert_eq!(value.get("name").and_then(|v| v.as_str()), Some("test"));
//!
//! // Typed via serde
//! use serde::Deserialize;
//!
//! #[derive(Deserialize)]
//! struct Config {
//!     name: String,
//!     port: u16,
//! }
//!
//! let config: Config = kvl::from_str("name = test\nport = 8080").unwrap();
//! assert_eq!(config.port, 8080);
//! ```
//!
//! ## Data model
//!
//! All values are strings at the parse level; type inference happens at the
//! application layer (here: the serde boundary). Repeated keys create nested
//! categorical structure, which [`loads`] compacts into lists:
//!
//! ```
//! use kvl::Value;
//!
//! let value = kvl::loads("tags = web\ntags = api").unwrap();
//! assert_eq!(
//!     value.get("tags").and_then(|v| v.as_list()).map(|l| l.len()),
//!     Some(2)
//! );
//! ```

mod config;
mod de;
mod error;
mod parser;
mod ser;
mod serializer;
mod transform;
mod util;
mod value;

pub use config::{
    auto_config_for_separator, extract_content, generate_header, parse_header, KvlConfig,
};
pub use de::{from_str, from_value};
pub use error::{Diagnostic, KvlError, Severity};
pub use parser::{
    loads, loads_with_config, loads_with_diagnostics, parse, parse_with_config,
    parse_with_diagnostics, MAX_INPUT_SIZE, MAX_RECURSION_DEPTH,
};
pub use ser::{to_string, to_string_with, to_value};
pub use serializer::{dumps, dumps_with, DumpOptions};
pub use transform::{compact, expand, merge};
pub use value::{Map, Value};

use std::path::Path;

/// Parse KVL from a file into the compacted user-facing model.
pub fn load(path: impl AsRef<Path>) -> Result<Value, KvlError> {
    let text = std::fs::read_to_string(path)
        .map_err(|e| KvlError::Message(format!("failed to read file: {e}")))?;
    loads(&text)
}

/// Serialize data to KVL text and write it to a file.
pub fn dump(data: &Value, path: impl AsRef<Path>) -> Result<(), KvlError> {
    let text = dumps(data)?;
    std::fs::write(path, text).map_err(|e| KvlError::Message(format!("failed to write file: {e}")))
}
