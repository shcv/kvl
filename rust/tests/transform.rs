use kvl::{compact, loads, merge, parse, KvlConfig, Map, Value};

fn s(v: &str) -> Value {
    Value::String(v.to_string())
}

fn parse_merge(a: &str, b: &str) -> Value {
    merge(&parse(a).unwrap(), &parse(b).unwrap())
}

#[test]
fn later_value_merges_into_categorical() {
    let merged = parse_merge("port = 8080", "port = 8081");
    let port = merged.get("port").unwrap().as_map().unwrap();
    assert!(port.contains_key("8080"));
    assert!(port.contains_key("8081"));
}

#[test]
fn objects_merge_recursively() {
    let merged = parse_merge("server =\n  host = a", "server =\n  port = 1");
    let server = merged.get("server").unwrap();
    assert!(server.get("host").is_some());
    assert!(server.get("port").is_some());
}

#[test]
fn empty_map_is_identity() {
    let a = parse("x = 1\ny =\n  z = 2").unwrap();
    assert_eq!(merge(&Value::empty(), &a), a);
    assert_eq!(merge(&a, &Value::empty()), a);
}

#[test]
fn merge_is_associative() {
    let a = parse("x = 1\nshared = a").unwrap();
    let b = parse("y = 2\nshared = b").unwrap();
    let c = parse("z = 3\nshared = c").unwrap();
    let left = merge(&merge(&a, &b), &c);
    let right = merge(&a, &merge(&b, &c));
    assert_eq!(left, right);
}

#[test]
fn merge_is_idempotent_on_identical_input() {
    let a = parse("x = 1\ntags = web\ntags = api").unwrap();
    assert_eq!(merge(&a, &a), a);
}

#[test]
fn compact_single_empty_child_to_string() {
    let config = KvlConfig::default();
    let mut inner = Map::new();
    inner.insert("test".to_string(), Value::empty());
    let mut outer = Map::new();
    outer.insert("name".to_string(), Value::Map(inner));
    let compacted = compact(&Value::Map(outer), &config);
    assert_eq!(compacted.get("name"), Some(&s("test")));
}

#[test]
fn compact_all_empty_values_to_list() {
    let config = KvlConfig::default();
    let mut inner = Map::new();
    inner.insert("a".to_string(), Value::empty());
    inner.insert("b".to_string(), Value::empty());
    inner.insert("c".to_string(), Value::empty());
    let mut outer = Map::new();
    outer.insert("tags".to_string(), Value::Map(inner));
    let compacted = compact(&Value::Map(outer), &config);
    assert_eq!(
        compacted.get("tags"),
        Some(&Value::List(vec![s("a"), s("b"), s("c")]))
    );
}

#[test]
fn compact_preserves_section_header_keys() {
    let config = KvlConfig::default();
    let mut inner = Map::new();
    inner.insert("== Section ==".to_string(), Value::empty());
    inner.insert("== Other ==".to_string(), Value::empty());
    let mut outer = Map::new();
    outer.insert("doc".to_string(), Value::Map(inner));
    let compacted = compact(&Value::Map(outer), &config);
    assert!(matches!(compacted.get("doc"), Some(Value::Map(_))));
}

#[test]
fn loads_equals_compact_of_parse() {
    let text = "server =\n  host = localhost\ntags = a\ntags = b";
    let via_loads = loads(text).unwrap();
    let via_parse = compact(&parse(text).unwrap(), &KvlConfig::default());
    assert_eq!(via_loads, via_parse);
}
