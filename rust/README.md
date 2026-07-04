# KVL for Rust

Rust implementation of KVL (Key-Value Language), a minimalist configuration
format with associative merge semantics. Based on the original CCL by
Dmitrii Kovanikov (chshersh): https://github.com/chshersh/ccl

## Features

- Full KVL parser: headers, custom separators, escapes, list markers,
  continuation/multiline semantics, W001/W002 diagnostics, strict mode
- Serializer with categorical, list-marker, and public formats
- Categorical merge, compact, and expand transforms
- Serde integration: typed `from_str`/`to_string` with type inference, plus
  `Serialize`/`Deserialize` on the dynamic `Value` type for JSON interop
- CLI with the standard cross-implementation commands
- Input size (10 MB) and nesting depth (100) limits

## Library usage

Untyped, via the dynamic `Value` model:

```rust
let value = kvl::loads("name = test\nport = 8080")?;
assert_eq!(value.get("name").and_then(|v| v.as_str()), Some("test"));

let text = kvl::dumps(&value)?;
```

Typed, via serde (type inference happens at the serde boundary — all KVL
values are strings at the parse level):

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct Config {
    name: String,
    port: u16,
    active: bool,
    tags: Vec<String>,
}

let config: Config = kvl::from_str("name = app\nport = 8080\nactive = true\ntags = web\ntags = api")?;
let text = kvl::to_string(&config)?;
```

Merging follows the categorical monoid semantics shared by all
implementations (`(A + B) + C == A + (B + C)`):

```rust
let base = kvl::parse("port = 8080")?;
let overlay = kvl::parse("port = 8081")?;
let merged = kvl::merge(&base, &overlay);
let compacted = kvl::compact(&merged, &kvl::KvlConfig::default());
// port = ["8080", "8081"]
```

Lower-level APIs: `parse` (raw categorical model), `loads_with_diagnostics`
(surface W001/W002 warnings), `parse_header` / `KvlConfig` (custom separators
and options), `dumps_with` / `DumpOptions` (headers, list markers, public
format).

## CLI

```bash
cargo build --release

./target/release/kvl parse config.kvl        # compacted JSON
./target/release/kvl parse-raw config.kvl    # categorical JSON
./target/release/kvl serialize < data.json   # JSON stdin -> KVL stdout
./target/release/kvl merge base.kvl overlay.kvl
```

## Tests

```bash
cargo test                       # unit, integration, and doc tests
python ../conformance/run.py     # cross-implementation conformance (from repo root)
```
