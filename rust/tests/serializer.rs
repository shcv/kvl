use kvl::{
    auto_config_for_separator, dumps, dumps_with, generate_header, loads, DumpOptions, KvlConfig,
    Map, Value,
};

fn s(v: &str) -> Value {
    Value::String(v.to_string())
}

fn map(entries: &[(&str, Value)]) -> Value {
    let mut m = Map::new();
    for (k, v) in entries {
        m.insert(k.to_string(), v.clone());
    }
    Value::Map(m)
}

fn round_trip(text: &str) {
    let parsed = loads(text).unwrap();
    let serialized = dumps(&parsed).unwrap();
    let reparsed = loads(&serialized).unwrap();
    assert_eq!(parsed, reparsed, "round-trip mismatch for {text:?}");
}

#[test]
fn empty_map_serializes_to_empty_string() {
    assert_eq!(dumps(&Value::empty()).unwrap(), "");
}

#[test]
fn round_trip_simple() {
    round_trip("name = test\nage = 30");
}

#[test]
fn round_trip_nested() {
    round_trip("server =\n  host = localhost\n  port = 8080");
}

#[test]
fn round_trip_lists() {
    round_trip("tags = red\ntags = green\ntags = blue");
}

#[test]
fn round_trip_multiline() {
    round_trip("description =\n    line one\n    line two\nname = test");
}

#[test]
fn round_trip_escaped_separator() {
    round_trip("equation = E \\= mc^2");
}

#[test]
fn round_trip_empty_value() {
    round_trip("name = test\nempty =");
}

#[test]
fn header_generation() {
    let config = auto_config_for_separator(":");
    assert_eq!(generate_header(&config), "#: kvl 1.0");

    let config = KvlConfig {
        list_markers: "-+".to_string(),
        ..KvlConfig::default()
    };
    assert_eq!(generate_header(&config), "#= kvl 1.0 -+");
}

#[test]
fn include_header_prepends_header_line() {
    let config = auto_config_for_separator(":");
    let options = DumpOptions {
        include_header: true,
        ..DumpOptions::default()
    };
    let data = map(&[("name", s("test"))]);
    let text = dumps_with(&data, &config, &options).unwrap();
    assert!(text.starts_with("#: kvl 1.0\n"));
    let reparsed = loads(&text).unwrap();
    assert_eq!(reparsed.get("name"), Some(&s("test")));
}

#[test]
fn colon_separator_spacing() {
    let config = auto_config_for_separator(":");
    let options = DumpOptions {
        public_format: true,
        ..DumpOptions::default()
    };
    let data = map(&[("name", s("test"))]);
    assert_eq!(dumps_with(&data, &config, &options).unwrap(), "name: test\n");
}

#[test]
fn list_marker_serialization_round_trips() {
    let config = KvlConfig {
        list_markers: "-".to_string(),
        ..KvlConfig::default()
    };
    let data = map(&[("items", Value::List(vec![s("alpha"), s("beta")]))]);
    let options = DumpOptions {
        include_header: true,
        ..DumpOptions::default()
    };
    let text = dumps_with(&data, &config, &options).unwrap();
    assert!(text.contains("- alpha"));
    let reparsed = loads(&text).unwrap();
    assert_eq!(
        reparsed.get("items"),
        Some(&Value::List(vec![s("alpha"), s("beta")]))
    );
}

#[test]
fn public_format_scalar_values_inline() {
    let options = DumpOptions {
        public_format: true,
        ..DumpOptions::default()
    };
    let data = map(&[("name", s("test")), ("port", s("8080"))]);
    let text = dumps_with(&data, &KvlConfig::default(), &options).unwrap();
    assert_eq!(text, "name = test\nport = 8080\n");
}

#[test]
fn public_format_string_lists_as_repeated_keys() {
    let options = DumpOptions {
        public_format: true,
        ..DumpOptions::default()
    };
    let data = map(&[("tags", Value::List(vec![s("a"), s("b")]))]);
    let text = dumps_with(&data, &KvlConfig::default(), &options).unwrap();
    assert_eq!(text, "tags = a\ntags = b\n");
    let reparsed = loads(&text).unwrap();
    assert_eq!(reparsed.get("tags"), Some(&Value::List(vec![s("a"), s("b")])));
}

#[test]
fn categorical_default_serializes_lists_via_expand() {
    let data = map(&[("tags", Value::List(vec![s("a"), s("b")]))]);
    let text = dumps(&data).unwrap();
    let reparsed = loads(&text).unwrap();
    assert_eq!(reparsed.get("tags"), Some(&Value::List(vec![s("a"), s("b")])));
}

#[test]
fn top_level_non_map_is_error() {
    assert!(dumps(&s("just a string")).is_err());
    assert!(dumps(&Value::List(vec![s("a")])).is_err());
}
