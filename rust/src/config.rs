//! Configuration for KVL parsing and serialization, including header handling.
//!
//! Header format: `#<separator> kvl <version> [list_markers] [options]`
//! Examples: `#= kvl 1.0`, `#: kvl 1.0 -`, `#:= kvl 1.0 strict`

use crate::error::KvlError;
use crate::util::split_lines;

/// Configuration for KVL parsing and serialization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KvlConfig {
    pub separator: String,
    pub version: String,
    pub compact: bool,
    pub space_before: bool,
    pub space_after: bool,
    /// Characters that mark list items (e.g. "-+*"); empty disables markers.
    pub list_markers: String,
    /// When true, W001/W002 diagnostics become parse errors.
    pub strict: bool,
}

impl Default for KvlConfig {
    fn default() -> Self {
        KvlConfig {
            separator: "=".to_string(),
            version: "1.0".to_string(),
            compact: false,
            space_before: true,
            space_after: true,
            list_markers: String::new(),
            strict: false,
        }
    }
}

impl KvlConfig {
    /// Validate the separator: non-empty and free of whitespace.
    pub fn validate(&self) -> Result<(), KvlError> {
        if self.separator.is_empty() {
            return Err(KvlError::parse("Separator cannot be empty", None, None));
        }
        if self
            .separator
            .chars()
            .any(|c| matches!(c, ' ' | '\t' | '\n' | '\r'))
        {
            return Err(KvlError::parse(
                "Separator cannot contain whitespace characters",
                None,
                None,
            ));
        }
        Ok(())
    }
}

/// Create a `KvlConfig` with spacing conventions appropriate for the separator.
/// Colon gets JSON/YAML-style spacing (no space before, space after); all
/// other separators get a space on both sides.
pub fn auto_config_for_separator(separator: &str) -> KvlConfig {
    let (space_before, space_after) = if separator == ":" {
        (false, true)
    } else {
        (true, true)
    };
    KvlConfig {
        separator: separator.to_string(),
        space_before,
        space_after,
        ..KvlConfig::default()
    }
}

#[derive(Debug, Clone)]
enum OptValue {
    Bool(bool),
    Str(String),
}

impl OptValue {
    fn truthy(&self) -> bool {
        match self {
            OptValue::Bool(b) => *b,
            OptValue::Str(s) => !s.is_empty(),
        }
    }
}

fn coerce(value: &str) -> OptValue {
    match value.to_lowercase().as_str() {
        "true" => OptValue::Bool(true),
        "false" => OptValue::Bool(false),
        _ => OptValue::Str(value.to_string()),
    }
}

/// Match `#<separator>\s+kvl(\s+<rest>)?` against a header line (after the
/// leading `#`). Returns `(separator, rest)` on success.
fn match_header(after_hash: &str) -> Option<(&str, &str)> {
    let mut idx = 0;
    while let Some(off) = after_hash[idx..].find("kvl") {
        let pos = idx + off;
        let after_kvl = &after_hash[pos + 3..];
        let follows_ok =
            after_kvl.is_empty() || after_kvl.starts_with(|c: char| c.is_whitespace());
        if follows_ok && pos > 0 {
            let before = &after_hash[..pos];
            let separator = before.trim_end();
            if !separator.is_empty() && separator.len() < before.len() {
                return Some((separator, after_kvl.trim_start()));
            }
        }
        idx = pos + 1;
    }
    None
}

/// Parse a KVL header from the first line of `text`. Returns `Ok(None)` when
/// no header is present, and an error for a malformed header (e.g. missing
/// version).
pub fn parse_header(text: &str) -> Result<Option<KvlConfig>, KvlError> {
    let lines = split_lines(text);
    let first_line = match lines.first() {
        Some(l) => l.trim(),
        None => return Ok(None),
    };
    if !first_line.starts_with('#') {
        return Ok(None);
    }
    let (separator, rest) = match match_header(&first_line[1..]) {
        Some(m) => m,
        None => return Ok(None),
    };

    let parts: Vec<&str> = rest.split_whitespace().collect();
    if parts.is_empty() {
        return Err(KvlError::parse("Missing version in KVL header", None, None));
    }
    let version = parts[0];

    let mut list_markers = "";
    let option_parts: &[&str] = if parts.len() > 1 {
        let second = parts[1];
        let is_alnum = !second.is_empty() && second.chars().all(|c| c.is_alphanumeric());
        if !second.contains('=') && !is_alnum {
            list_markers = second;
            &parts[2..]
        } else {
            &parts[1..]
        }
    } else {
        &[]
    };

    let mut options: Vec<(String, OptValue)> = Vec::new();
    for part in option_parts {
        if let Some(eq) = part.find('=') {
            let key = part[..eq].replace('-', "_");
            options.push((key, coerce(&part[eq + 1..])));
        } else {
            options.push((part.replace('-', "_"), OptValue::Bool(true)));
        }
    }

    fn find_opt<'a>(options: &'a [(String, OptValue)], key: &str) -> Option<&'a OptValue> {
        options.iter().find(|(k, _)| k == key).map(|(_, v)| v)
    }

    if let Some(around) = find_opt(&options, "space_around").cloned() {
        let around = around.truthy();
        if find_opt(&options, "space_before").is_none() {
            options.push(("space_before".to_string(), OptValue::Bool(around)));
        }
        if find_opt(&options, "space_after").is_none() {
            options.push(("space_after".to_string(), OptValue::Bool(around)));
        }
    }

    let strict = find_opt(&options, "strict").map(|v| v.truthy()).unwrap_or(false);
    let compact = find_opt(&options, "compact").map(|v| v.truthy()).unwrap_or(false);
    let auto = auto_config_for_separator(separator);
    let config = KvlConfig {
        separator: separator.to_string(),
        version: version.to_string(),
        compact,
        space_before: find_opt(&options, "space_before")
            .map(|v| v.truthy())
            .unwrap_or(auto.space_before),
        space_after: find_opt(&options, "space_after")
            .map(|v| v.truthy())
            .unwrap_or(auto.space_after),
        list_markers: list_markers.to_string(),
        strict,
    };
    config.validate()?;
    Ok(Some(config))
}

/// Generate a KVL header line (without trailing newline) from a configuration.
pub fn generate_header(config: &KvlConfig) -> String {
    let mut header = format!("#{} kvl {}", config.separator, config.version);
    if !config.list_markers.is_empty() {
        header.push(' ');
        header.push_str(&config.list_markers);
    }
    if config.compact {
        header.push_str(" compact");
    }
    header
}

/// Strip the header line from KVL text if one is present.
pub fn extract_content(text: &str) -> String {
    let lines = split_lines(text);
    let first_line = match lines.first() {
        Some(l) => l.trim(),
        None => return text.to_string(),
    };
    if first_line.starts_with('#') && first_line.contains(" kvl ") {
        return lines[1..].join("\n");
    }
    text.to_string()
}
