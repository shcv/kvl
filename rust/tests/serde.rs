use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Server {
    host: String,
    port: u16,
    active: bool,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct AppConfig {
    name: String,
    server: Server,
    tags: Vec<String>,
}

#[test]
fn typed_deserialization_with_inference() {
    let config: Server = kvl::from_str("host = localhost\nport = 8080\nactive = true").unwrap();
    assert_eq!(
        config,
        Server {
            host: "localhost".to_string(),
            port: 8080,
            active: true,
        }
    );
}

#[test]
fn nested_struct_round_trip() {
    let config = AppConfig {
        name: "myapp".to_string(),
        server: Server {
            host: "example.com".to_string(),
            port: 443,
            active: false,
        },
        tags: vec!["web".to_string(), "api".to_string()],
    };
    let text = kvl::to_string(&config).unwrap();
    let back: AppConfig = kvl::from_str(&text).unwrap();
    assert_eq!(back, config);
}

#[test]
fn single_element_list_survives_compaction() {
    // Compaction collapses one-element lists to scalars; the deserializer
    // recovers them.
    #[derive(Debug, PartialEq, Deserialize)]
    struct Tagged {
        tags: Vec<String>,
    }
    let config: Tagged = kvl::from_str("tags = solo").unwrap();
    assert_eq!(config.tags, ["solo"]);
}

#[test]
fn numbers_parse_from_strings() {
    #[derive(Debug, PartialEq, Deserialize)]
    struct Nums {
        int: i64,
        float: f64,
        negative: i32,
    }
    let n: Nums = kvl::from_str("int = 42\nfloat = 2.5\nnegative = -7").unwrap();
    assert_eq!(n.int, 42);
    assert_eq!(n.float, 2.5);
    assert_eq!(n.negative, -7);
}

#[test]
fn invalid_number_is_error() {
    #[derive(Debug, Deserialize)]
    #[allow(dead_code)]
    struct Nums {
        int: i64,
    }
    assert!(kvl::from_str::<Nums>("int = not-a-number").is_err());
}

#[test]
fn optional_fields() {
    #[derive(Debug, PartialEq, Deserialize)]
    struct WithOpt {
        name: String,
        nickname: Option<String>,
    }
    let v: WithOpt = kvl::from_str("name = sam").unwrap();
    assert_eq!(v.nickname, None);
    let v: WithOpt = kvl::from_str("name = sam\nnickname = sc").unwrap();
    assert_eq!(v.nickname, Some("sc".to_string()));
}

#[test]
fn unit_enum_variants() {
    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    #[serde(rename_all = "lowercase")]
    enum Level {
        Debug,
        Info,
        Warn,
    }
    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct Logging {
        level: Level,
    }
    let v: Logging = kvl::from_str("level = info").unwrap();
    assert_eq!(v.level, Level::Info);
    let text = kvl::to_string(&Logging { level: Level::Warn }).unwrap();
    assert_eq!(text, "level = warn\n");
    let _ = Level::Debug;
}

#[test]
fn maps_deserialize_with_string_values() {
    use std::collections::BTreeMap;
    let v: BTreeMap<String, String> = kvl::from_str("a = 1\nb = 2").unwrap();
    assert_eq!(v["a"], "1");
    assert_eq!(v["b"], "2");
}

#[test]
fn untyped_value_deserialization() {
    let v: kvl::Value = kvl::from_str("name = test").unwrap();
    assert_eq!(v.get("name").and_then(|x| x.as_str()), Some("test"));
}

#[test]
fn to_value_stringifies_scalars() {
    #[derive(Serialize)]
    struct Mixed {
        n: u32,
        b: bool,
        s: &'static str,
    }
    let v = kvl::to_value(&Mixed {
        n: 7,
        b: true,
        s: "x",
    })
    .unwrap();
    assert_eq!(v.get("n").and_then(|x| x.as_str()), Some("7"));
    assert_eq!(v.get("b").and_then(|x| x.as_str()), Some("true"));
    assert_eq!(v.get("s").and_then(|x| x.as_str()), Some("x"));
}

#[test]
fn multiline_string_round_trip() {
    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct Doc {
        description: String,
        name: String,
    }
    let doc = Doc {
        description: "line one\nline two".to_string(),
        name: "test".to_string(),
    };
    let text = kvl::to_string(&doc).unwrap();
    let back: Doc = kvl::from_str(&text).unwrap();
    assert_eq!(back, doc);
}

#[test]
fn json_interop_via_serde() {
    // kvl::Value implements Serialize/Deserialize, so it converts to and
    // from JSON directly.
    let v = kvl::loads("server =\n  host = localhost\ntags = a\ntags = b").unwrap();
    let json = serde_json::to_string(&v).unwrap();
    assert_eq!(
        json,
        r#"{"server":{"host":"localhost"},"tags":["a","b"]}"#
    );
    let back: kvl::Value = serde_json::from_str(&json).unwrap();
    assert_eq!(back, v);
}
