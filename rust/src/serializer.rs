//! Serializer for Key-Value Language (KVL).
//!
//! Port of the Python reference implementation (`python/kvl/serializer.py`).

use crate::config::{generate_header, KvlConfig};
use crate::error::KvlError;
use crate::transform::expand;
use crate::util::{escape_separator, is_blank, line_indent};
use crate::value::Value;

/// Options controlling KVL serialization.
#[derive(Debug, Clone)]
pub struct DumpOptions {
    /// Emit a `#<sep> kvl <version>` header line.
    pub include_header: bool,
    /// List marker for serializing lists. `None` auto-detects from
    /// `config.list_markers`; `Some("")` forces categorical format.
    pub list_marker: Option<String>,
    /// Serialize user-facing scalar values directly and encode string lists
    /// as repeated keys when no list marker is available.
    pub public_format: bool,
    /// Indentation unit (default two spaces).
    pub indent: String,
}

impl Default for DumpOptions {
    fn default() -> Self {
        DumpOptions {
            include_header: false,
            list_marker: None,
            public_format: false,
            indent: "  ".to_string(),
        }
    }
}

/// Serialize data to KVL text with default configuration and options.
pub fn dumps(data: &Value) -> Result<String, KvlError> {
    dumps_with(data, &KvlConfig::default(), &DumpOptions::default())
}

/// Serialize data to KVL text.
pub fn dumps_with(
    data: &Value,
    config: &KvlConfig,
    options: &DumpOptions,
) -> Result<String, KvlError> {
    let map = match data {
        Value::Map(m) => m,
        _ => {
            return Err(KvlError::serialize(
                "Top-level KVL value must be a map".to_string(),
            ))
        }
    };

    if map.is_empty() {
        return Ok(if options.include_header {
            format!("{}\n", generate_header(config))
        } else {
            String::new()
        });
    }

    let effective_marker = determine_list_marker(options.list_marker.as_deref(), config);

    let result = if effective_marker.is_some() || options.public_format {
        let mut serializer = Serializer::new(config, options, effective_marker);
        serializer.serialize(data)?
    } else {
        let expanded = expand(data);
        let mut serializer = Serializer::new(config, options, None);
        serializer.serialize(&expanded)?
    };

    Ok(if options.include_header {
        format!("{}\n{}", generate_header(config), result)
    } else {
        result
    })
}

fn determine_list_marker(list_marker: Option<&str>, config: &KvlConfig) -> Option<String> {
    if let Some(marker) = list_marker {
        // Explicit override; empty string means no list markers.
        return if marker.is_empty() {
            None
        } else {
            Some(marker.to_string())
        };
    }
    config
        .list_markers
        .chars()
        .next()
        .map(|c| c.to_string())
}

struct Serializer<'a> {
    config: &'a KvlConfig,
    indent: &'a str,
    list_marker: Option<String>,
    public_format: bool,
    lines: Vec<String>,
}

impl<'a> Serializer<'a> {
    fn new(config: &'a KvlConfig, options: &'a DumpOptions, list_marker: Option<String>) -> Self {
        Serializer {
            config,
            indent: &options.indent,
            list_marker,
            public_format: options.public_format,
            lines: Vec::new(),
        }
    }

    fn serialize(&mut self, data: &Value) -> Result<String, KvlError> {
        let map = match data {
            Value::Map(m) => m,
            _ => {
                return Err(KvlError::serialize(
                    "Top-level KVL value must be a map".to_string(),
                ))
            }
        };
        if map.is_empty() {
            return Ok(String::new());
        }
        self.serialize_map(data, 0)?;
        let mut result = self.lines.join("\n");
        if !result.is_empty() {
            result.push('\n');
        }
        Ok(result)
    }

    fn escape_text(&self, text: &str) -> String {
        escape_separator(text, &self.config.separator)
    }

    fn format_separator(&self, for_empty: bool) -> String {
        let before = if self.config.space_before { " " } else { "" };
        let after = if for_empty {
            ""
        } else if self.config.space_after {
            " "
        } else {
            ""
        };
        format!("{before}{}{after}", self.config.separator)
    }

    fn serialize_map(&mut self, data: &Value, level: usize) -> Result<(), KvlError> {
        let map = data.as_map().expect("serialize_map requires a map");
        let indent_str = self.indent.repeat(level);

        for (key, value) in map {
            match value {
                Value::Map(_) => self.serialize_map_value(key, value, &indent_str, level)?,
                Value::List(items) => {
                    self.serialize_list_value(key, items, &indent_str, level)?
                }
                Value::String(s) => self.serialize_string_value(key, s, &indent_str, level),
            }
        }
        Ok(())
    }

    fn serialize_map_value(
        &mut self,
        key: &str,
        value: &Value,
        indent_str: &str,
        level: usize,
    ) -> Result<(), KvlError> {
        let sep_str = self.format_separator(true);
        let escaped_key = self.escape_text(key);
        self.lines.push(format!("{indent_str}{escaped_key}{sep_str}"));
        if !value.is_empty_map() {
            self.serialize_map(value, level + 1)?;
        }
        Ok(())
    }

    fn serialize_list_value(
        &mut self,
        key: &str,
        items: &[Value],
        indent_str: &str,
        level: usize,
    ) -> Result<(), KvlError> {
        if self.list_marker.is_none() {
            if self.public_format {
                for item in items {
                    match item {
                        Value::String(s) => {
                            self.serialize_string_value(key, s, indent_str, level)
                        }
                        _ => {
                            return Err(KvlError::serialize(
                                "Complex list items require a configured list marker \
                                 in public_format mode."
                                    .to_string(),
                            ))
                        }
                    }
                }
                return Ok(());
            }
            return Err(KvlError::serialize(
                "List values are not supported without list markers. \
                 Use expand() to convert to categorical format."
                    .to_string(),
            ));
        }
        let sep_str = self.format_separator(true);
        let escaped_key = self.escape_text(key);
        self.lines.push(format!("{indent_str}{escaped_key}{sep_str}"));
        self.serialize_list(items, level + 1)
    }

    fn serialize_string_value(&mut self, key: &str, value: &str, indent_str: &str, level: usize) {
        let escaped_key = self.escape_text(key);
        if value.is_empty() {
            let sep_str = self.format_separator(true);
            self.lines.push(format!("{indent_str}{escaped_key}{sep_str}"));
        } else if value.contains('\n') {
            self.serialize_multiline_value(&escaped_key, value, indent_str, level);
        } else {
            let escaped_value = self.escape_text(value);
            let sep_str = self.format_separator(false);
            self.lines
                .push(format!("{indent_str}{escaped_key}{sep_str}{escaped_value}"));
        }
    }

    fn serialize_multiline_value(
        &mut self,
        escaped_key: &str,
        value: &str,
        indent_str: &str,
        level: usize,
    ) {
        let parent_indent_len = indent_str.len();

        if let Some(rest) = value.strip_prefix('\n') {
            // Empty-key continuation: value is \n followed by indented lines.
            let content_lines: Vec<&str> = rest.split('\n').collect();
            let non_blank: Vec<&str> = content_lines
                .iter()
                .filter(|l| !is_blank(l))
                .copied()
                .collect();
            let min_indent = non_blank.iter().map(|l| line_indent(l)).min().unwrap_or(0);
            let sep_str = self.format_separator(true);
            self.lines.push(format!("{indent_str}{escaped_key}{sep_str}"));
            if !non_blank.is_empty() && min_indent > parent_indent_len {
                // Original indentation is valid; preserve it.
                for line in content_lines {
                    self.lines.push(line.to_string());
                }
            } else {
                // Re-indent canonically.
                let child_indent = self.indent.repeat(level + 1);
                for line in content_lines {
                    self.lines.push(format!("{child_indent}{line}"));
                }
            }
        } else {
            // Valued-key continuation: first line is inline, rest are indented.
            let all_lines: Vec<&str> = value.split('\n').collect();
            let cont_lines = &all_lines[1..];
            let non_blank: Vec<&str> =
                cont_lines.iter().filter(|l| !is_blank(l)).copied().collect();
            let min_indent = non_blank.iter().map(|l| line_indent(l)).min().unwrap_or(0);
            if !non_blank.is_empty() && min_indent > parent_indent_len {
                // Continuation lines have valid indentation; preserve as
                // valued-key form.
                let sep_str = self.format_separator(false);
                self.lines.push(format!(
                    "{indent_str}{escaped_key}{sep_str}{}",
                    self.escape_text(all_lines[0])
                ));
                for line in cont_lines {
                    self.lines.push(line.to_string());
                }
            } else {
                // Re-indent canonically using the empty-key form.
                let sep_str = self.format_separator(true);
                self.lines.push(format!("{indent_str}{escaped_key}{sep_str}"));
                let child_indent = self.indent.repeat(level + 1);
                for line in all_lines {
                    self.lines.push(format!("{child_indent}{line}"));
                }
            }
        }
    }

    fn serialize_list(&mut self, items: &[Value], level: usize) -> Result<(), KvlError> {
        let indent_str = self.indent.repeat(level);
        let marker = self.list_marker.clone().expect("list marker required");
        let marker_str = format!("{marker} ");

        for item in items {
            match item {
                Value::String(s) => {
                    let escaped_item = self.escape_text(s);
                    self.lines
                        .push(format!("{indent_str}{marker_str}{escaped_item}"));
                }
                Value::List(nested) => {
                    // Nested list item: bare marker plus nested list block.
                    self.lines.push(format!("{indent_str}{marker}"));
                    self.serialize_list(nested, level + 1)?;
                }
                Value::Map(map) => {
                    if map.len() != 1 {
                        self.lines.push(format!("{indent_str}{marker}"));
                        self.serialize_map(item, level + 1)?;
                        continue;
                    }
                    let (key, value) = map.iter().next().unwrap();
                    match value {
                        Value::String(s) if !s.contains('\n') => {
                            let sep_str = self.format_separator(false);
                            let escaped_key = self.escape_text(key);
                            let escaped_value = self.escape_text(s);
                            self.lines.push(format!(
                                "{indent_str}{marker_str}{escaped_key}{sep_str}{escaped_value}"
                            ));
                        }
                        _ => {
                            let sep_str = self.format_separator(true);
                            let escaped_key = self.escape_text(key);
                            self.lines
                                .push(format!("{indent_str}{marker_str}{escaped_key}{sep_str}"));
                            match value {
                                Value::Map(_) => self.serialize_map(value, level + 1)?,
                                Value::List(nested) => {
                                    self.serialize_list(nested, level + 1)?
                                }
                                Value::String(s) => {
                                    // Multiline string value under a list item.
                                    self.lines
                                        .push(format!("{}{s}", self.indent.repeat(level + 1)));
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
