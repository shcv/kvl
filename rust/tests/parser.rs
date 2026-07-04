use kvl::{loads, loads_with_diagnostics, parse, KvlError, Value};

fn s(v: &str) -> Value {
    Value::String(v.to_string())
}

fn get<'a>(value: &'a Value, key: &str) -> &'a Value {
    value.get(key).unwrap_or_else(|| panic!("missing key {key:?}"))
}

#[test]
fn simple_key_value() {
    let v = loads("name = test").unwrap();
    assert_eq!(get(&v, "name"), &s("test"));
}

#[test]
fn empty_input_parses_to_empty_map() {
    assert_eq!(loads("").unwrap(), Value::empty());
    assert_eq!(loads("   \n  \n").unwrap(), Value::empty());
    assert_eq!(parse("").unwrap(), Value::empty());
}

#[test]
fn values_are_strings_at_parse_level() {
    let v = loads("age = 30\nactive = true\npi = 3.14").unwrap();
    assert_eq!(get(&v, "age"), &s("30"));
    assert_eq!(get(&v, "active"), &s("true"));
    assert_eq!(get(&v, "pi"), &s("3.14"));
}

#[test]
fn nested_objects_via_indentation() {
    let v = loads("server =\n    host = localhost\n    port = 8080").unwrap();
    let server = get(&v, "server");
    assert_eq!(get(server, "host"), &s("localhost"));
    assert_eq!(get(server, "port"), &s("8080"));
}

#[test]
fn repeated_keys_compact_to_list() {
    let v = loads("tags = red\ntags = green\ntags = blue").unwrap();
    assert_eq!(
        get(&v, "tags"),
        &Value::List(vec![s("red"), s("green"), s("blue")])
    );
}

#[test]
fn repeated_keys_categorical_structure() {
    let v = parse("tags = web\ntags = api").unwrap();
    let tags = get(&v, "tags").as_map().unwrap();
    assert!(tags.contains_key("web"));
    assert!(tags.contains_key("api"));
    assert!(tags["web"].is_empty_map());
}

#[test]
fn comments_are_bare_keys_with_comment_prefix() {
    let v = loads("/= a comment\nname = test").unwrap();
    assert_eq!(get(&v, "name"), &s("test"));
    assert!(v.get("/").is_some());
}

#[test]
fn separator_in_value_splits_on_first() {
    let v = loads("equation = a = b").unwrap();
    assert_eq!(get(&v, "equation"), &s("a = b"));
}

#[test]
fn escaped_separator_in_key() {
    let v = loads("a\\=b = value").unwrap();
    assert_eq!(get(&v, "a=b"), &s("value"));
}

#[test]
fn escape_decrement_double_backslash() {
    // \\= : first backslash literal, second escapes the separator.
    let v = loads("key = x\\\\=y").unwrap();
    assert_eq!(get(&v, "key"), &s("x\\=y"));
}

#[test]
fn bare_key_is_parse_error() {
    let err = loads("just-a-key").unwrap_err();
    assert!(matches!(err, KvlError::Parse { .. }));
}

#[test]
fn missing_separator_reports_line() {
    let err = loads("a = 1\nbare").unwrap_err();
    match err {
        KvlError::Parse { line, .. } => assert_eq!(line, Some(2)),
        other => panic!("unexpected error {other:?}"),
    }
}

#[test]
fn mixed_indentation_rejected() {
    let text = "a =\n    x = 1\n\ty = 2";
    assert!(loads(text).is_err());
}

#[test]
fn depth_limit_enforced() {
    // 100 levels of nesting parse fine; 101 exceed the limit (matches the
    // Python reference exactly).
    fn deep(levels: usize) -> String {
        let mut text = String::new();
        for i in 0..levels {
            text.push_str(&"  ".repeat(i));
            text.push_str(&format!("k{i} =\n"));
        }
        text.push_str(&"  ".repeat(levels));
        text.push_str("leaf = value\n");
        text
    }
    assert!(loads(&deep(100)).is_ok());
    assert!(loads(&deep(101)).is_err());
}

#[test]
fn input_size_limit_enforced() {
    let big = "x".repeat(kvl::MAX_INPUT_SIZE + 1);
    assert!(loads(&big).is_err());
}

#[test]
fn multiline_empty_key_continuation() {
    let v = loads("description =\n    line one\n    line two").unwrap();
    assert_eq!(get(&v, "description"), &s("line one\nline two"));
}

#[test]
fn valued_key_with_children_is_continuation_with_w001() {
    let text = "server = primary\n    host = localhost\n    port = 8080";
    let (v, diags) = loads_with_diagnostics(text, None).unwrap();
    assert_eq!(
        get(&v, "server"),
        &s("primary\nhost = localhost\nport = 8080")
    );
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code, "W001");
}

#[test]
fn mixed_continuation_is_text_with_w002() {
    let text = "notes =\n    line one\n    key = value\n    another line";
    let (v, diags) = loads_with_diagnostics(text, None).unwrap();
    assert_eq!(get(&v, "notes"), &s("line one\nkey = value\nanother line"));
    assert!(diags.iter().any(|d| d.code == "W002"));
}

#[test]
fn strict_mode_makes_w001_an_error() {
    let text = "#= kvl 1.0 strict\nserver = primary\n    host = localhost";
    assert!(loads(text).is_err());
}

#[test]
fn strict_mode_makes_w002_an_error() {
    let text = "#= kvl 1.0 strict\nnotes =\n    plain text\n    key = value";
    assert!(loads(text).is_err());
}

#[test]
fn all_separator_lines_make_nested_kvl() {
    let text = "config =\n    a = 1\n    b = 2";
    let v = loads(text).unwrap();
    let config = get(&v, "config");
    assert_eq!(get(config, "a"), &s("1"));
    assert_eq!(get(config, "b"), &s("2"));
}

#[test]
fn header_custom_separator() {
    let v = loads("#: kvl 1.0\nname: test\nport: 8080").unwrap();
    assert_eq!(get(&v, "name"), &s("test"));
    assert_eq!(get(&v, "port"), &s("8080"));
}

#[test]
fn header_arrow_separator() {
    let v = loads("#-> kvl 1.0\nname -> test").unwrap();
    assert_eq!(get(&v, "name"), &s("test"));
}

#[test]
fn header_list_markers() {
    let text = "#= kvl 1.0 -\nitems =\n    - alpha\n    - beta";
    let v = loads(text).unwrap();
    assert_eq!(get(&v, "items"), &Value::List(vec![s("alpha"), s("beta")]));
}

#[test]
fn header_missing_version_is_error() {
    assert!(loads("#= kvl\nname = test").is_err());
}

#[test]
fn unicode_keys_and_values() {
    let v = loads("café = 咖啡\nemoji = 🎉").unwrap();
    assert_eq!(get(&v, "café"), &s("咖啡"));
    assert_eq!(get(&v, "emoji"), &s("🎉"));
}

#[test]
fn blank_lines_between_entries_ignored() {
    let v = loads("a = 1\n\n\nb = 2").unwrap();
    assert_eq!(get(&v, "a"), &s("1"));
    assert_eq!(get(&v, "b"), &s("2"));
}

#[test]
fn key_order_preserved() {
    let v = loads("zebra = 1\napple = 2\nmango = 3").unwrap();
    let keys: Vec<&String> = v.as_map().unwrap().keys().collect();
    assert_eq!(keys, ["zebra", "apple", "mango"]);
}

#[test]
fn empty_value_stays_empty_map() {
    let v = loads("name = test\nempty =").unwrap();
    assert_eq!(get(&v, "empty"), &Value::empty());
}
