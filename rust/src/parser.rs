//! Parser for Key-Value Language (KVL).
//!
//! Faithful port of the Python reference implementation (`python/kvl/parser.py`),
//! which itself derives from the original CCL by Dmitrii Kovanikov (chshersh).

use crate::config::{extract_content, parse_header, KvlConfig};
use crate::error::{Diagnostic, KvlError, Severity};
use crate::transform::{compact, merge};
use crate::util::{find_unescaped_separator, is_blank, line_indent, split_lines, unescape_text};
use crate::value::{Map, Value};

/// Maximum nesting depth for structures (DoS protection).
pub const MAX_RECURSION_DEPTH: usize = 100;
/// Maximum input size in bytes (DoS protection).
pub const MAX_INPUT_SIZE: usize = 10 * 1024 * 1024;

/// Parse KVL text into the raw categorical model (low-level API).
///
/// Repeated keys appear as nested maps: `tags = web` + `tags = api` parses to
/// `{"tags": {"web": {}, "api": {}}}`. For the compacted user-facing form use
/// [`loads`].
pub fn parse(text: &str) -> Result<Value, KvlError> {
    parse_with_diagnostics(text, None).map(|(v, _)| v)
}

/// Like [`parse`], with an explicit configuration (skips header detection).
pub fn parse_with_config(text: &str, config: &KvlConfig) -> Result<Value, KvlError> {
    parse_with_diagnostics(text, Some(config)).map(|(v, _)| v)
}

/// Parse KVL text into user-friendly compacted data (standard API).
///
/// Repeated keys become lists and multiline continuation values are trimmed:
/// `tags = web` + `tags = api` loads as `{"tags": ["web", "api"]}`.
pub fn loads(text: &str) -> Result<Value, KvlError> {
    loads_with_diagnostics(text, None).map(|(v, _)| v)
}

/// Like [`loads`], with an explicit configuration (skips header detection).
pub fn loads_with_config(text: &str, config: &KvlConfig) -> Result<Value, KvlError> {
    loads_with_diagnostics(text, Some(config)).map(|(v, _)| v)
}

/// [`parse`] variant that also returns accumulated diagnostics (W001/W002).
pub fn parse_with_diagnostics(
    text: &str,
    config: Option<&KvlConfig>,
) -> Result<(Value, Vec<Diagnostic>), KvlError> {
    let config = resolve_config(text, config)?;
    let mut parser = Parser::new(&config);
    let value = parser.parse(text)?;
    Ok((value, parser.diagnostics))
}

/// [`loads`] variant that also returns accumulated diagnostics (W001/W002).
pub fn loads_with_diagnostics(
    text: &str,
    config: Option<&KvlConfig>,
) -> Result<(Value, Vec<Diagnostic>), KvlError> {
    let config = resolve_config(text, config)?;
    let mut parser = Parser::new(&config);
    let value = parser.parse(text)?;
    let compacted = compact(&value, &config);
    Ok((trim_multiline_values(&compacted), parser.diagnostics))
}

fn resolve_config(text: &str, config: Option<&KvlConfig>) -> Result<KvlConfig, KvlError> {
    match config {
        Some(c) => Ok(c.clone()),
        None => Ok(parse_header(text)?.unwrap_or_default()),
    }
}

enum Anon {
    Dict(String, String),
    Item(String),
}

#[derive(Default)]
struct KeyVals {
    kvs: Vec<(String, String)>,
    anons: Vec<Anon>,
}

enum BlockKind {
    Scalar,
    Nested,
    Text,
    Mixed,
}

struct Parser<'c> {
    config: &'c KvlConfig,
    diagnostics: Vec<Diagnostic>,
}

impl<'c> Parser<'c> {
    fn new(config: &'c KvlConfig) -> Self {
        Parser {
            config,
            diagnostics: Vec::new(),
        }
    }

    fn parse(&mut self, text: &str) -> Result<Value, KvlError> {
        if text.len() > MAX_INPUT_SIZE {
            return Err(KvlError::parse(
                format!("Input exceeds maximum size of {MAX_INPUT_SIZE} bytes"),
                None,
                None,
            ));
        }
        check_indent_consistency(text)?;
        self.config.validate()?;
        let key_vals = if is_blank(text) {
            KeyVals::default()
        } else {
            self.parse_kvs(text, false, 0)?
        };
        let model = self.build_model(key_vals, 0)?;
        Ok(unescape_model(&model, &self.config.separator))
    }

    fn emit_diagnostic(
        &mut self,
        code: &str,
        message: &str,
        line: Option<usize>,
    ) -> Result<(), KvlError> {
        if self.config.strict {
            return Err(KvlError::parse(format!("[{code}] {message}"), line, None));
        }
        self.diagnostics.push(Diagnostic {
            severity: Severity::Warning,
            code: code.to_string(),
            message: message.to_string(),
            line,
        });
        Ok(())
    }

    fn parse_kvs(
        &mut self,
        text: &str,
        allow_anonymous_lists: bool,
        depth: usize,
    ) -> Result<KeyVals, KvlError> {
        if depth > MAX_RECURSION_DEPTH {
            return Err(KvlError::parse(
                format!("Maximum nesting depth of {MAX_RECURSION_DEPTH} exceeded"),
                None,
                None,
            ));
        }
        let content = extract_content(text);
        let lines = split_lines(&content);
        let separator = &self.config.separator;
        let sep_len = separator.len();

        let mut result: Vec<(String, String)> = Vec::new();
        let mut anons: Vec<Anon> = Vec::new();
        let mut current_list_key: Option<String> = None;
        let mut i = 0;

        while i < lines.len() {
            let line = lines[i];
            if is_blank(line) {
                i += 1;
                continue;
            }

            let first_char_pos = line_indent(line);
            if is_list_marker(line, first_char_pos, self.config) {
                let marker_len = line[first_char_pos..]
                    .chars()
                    .next()
                    .map(|c| c.len_utf8())
                    .unwrap_or(1);
                let mut list_item_content = line[first_char_pos + marker_len..]
                    .trim_start()
                    .to_string();
                let parent_indent = line_indent(line);
                let (child_block, new_i) = capture_indented_block(&lines, i + 1, parent_indent);
                if !child_block.is_empty() {
                    if list_item_content.is_empty() {
                        list_item_content = child_block.clone();
                        i = new_i - 1;
                    } else if should_desugar_list_item_block(&list_item_content, separator) {
                        list_item_content =
                            desugar_list_item_block(&list_item_content, &child_block);
                        i = new_i - 1;
                    } else if current_list_key.is_some() {
                        list_item_content.push_str(&child_block);
                        i = new_i - 1;
                    }
                }

                let item_sep_pos = if !list_item_content.starts_with('\n') {
                    find_unescaped_separator(&list_item_content, separator)
                } else {
                    None
                };

                if let Some(key) = &current_list_key {
                    result.push((key.clone(), list_item_content));
                } else if allow_anonymous_lists {
                    if let Some(sep_pos) = item_sep_pos {
                        let item_key = list_item_content[..sep_pos].trim().to_string();
                        let mut item_value = list_item_content[sep_pos + sep_len..]
                            .trim()
                            .to_string();
                        if item_value.is_empty() && !child_block.is_empty() {
                            item_value = child_block.clone();
                            i = new_i - 1;
                        } else if item_value.is_empty() {
                            let (v, ni) = capture_indented_block(&lines, i + 1, parent_indent);
                            if !v.is_empty() {
                                item_value = v;
                                i = ni - 1;
                            }
                        }
                        anons.push(Anon::Dict(item_key, item_value));
                    } else {
                        anons.push(Anon::Item(list_item_content));
                    }
                } else {
                    return Err(KvlError::parse(
                        format!("List item found without preceding key at line {}", i + 1),
                        None,
                        None,
                    ));
                }

                i += 1;
                continue;
            }

            let indent = line_indent(line);
            let sep_pos = find_unescaped_separator(line, separator);

            match sep_pos {
                None if indent > 0 && !result.is_empty() => {
                    // Indented line with no separator: continuation of the
                    // previous value.
                    let last = result.last_mut().unwrap();
                    if last.1.is_empty() {
                        last.1 = format!("\n{}", line.trim_end());
                    } else {
                        last.1.push('\n');
                        last.1.push_str(line.trim_end());
                    }
                    i += 1;
                    continue;
                }
                None => {
                    return Err(KvlError::parse(
                        format!("Missing separator '{separator}' on line"),
                        Some(i + 1),
                        None,
                    ));
                }
                Some(sep_pos) => {
                    let key = line[..sep_pos].trim().to_string();
                    let mut value_part = line[sep_pos + sep_len..].trim().to_string();

                    // List items attach only to keys with empty (nested) values.
                    if value_part.is_empty() {
                        current_list_key = Some(key.clone());
                    } else {
                        current_list_key = None;
                    }

                    let parent_indent = indent;
                    if value_part.is_empty() {
                        let (v, new_i) = capture_indented_block(&lines, i + 1, parent_indent);
                        if !v.is_empty() {
                            value_part = v;
                            i = new_i - 1;
                        }
                    } else {
                        // Valued key with indented children: continuation text (W001)
                        let (cont, new_i) = capture_indented_block(&lines, i + 1, parent_indent);
                        if !cont.is_empty() {
                            self.emit_diagnostic(
                                "W001",
                                "Valued key with continuation: indented lines after \
                                 a valued key are treated as continuation text",
                                Some(i + 1),
                            )?;
                            value_part.push_str(&cont);
                            i = new_i - 1;
                        }
                    }

                    result.push((key, value_part));
                }
            }
            i += 1;
        }

        Ok(KeyVals {
            kvs: result,
            anons: if allow_anonymous_lists { anons } else { Vec::new() },
        })
    }

    fn classify_multiline_block(&self, value: &str) -> BlockKind {
        if !value.contains('\n') {
            return BlockKind::Scalar;
        }
        let lines: Vec<&str> = value.split('\n').collect();
        let non_blank: Vec<&str> = lines.iter().filter(|l| !is_blank(l)).copied().collect();
        if non_blank.is_empty() {
            return BlockKind::Text;
        }
        let min_indent = non_blank.iter().map(|l| line_indent(l)).min().unwrap();
        let base: Vec<&str> = non_blank
            .iter()
            .filter(|l| line_indent(l) == min_indent)
            .copied()
            .collect();

        let mut lines_with_sep = 0;
        for line in &base {
            let content = line.trim_start();
            let has_sep = find_unescaped_separator(content, &self.config.separator).is_some();
            let has_marker =
                !self.config.list_markers.is_empty() && is_list_marker(content, 0, self.config);
            if has_sep || has_marker {
                lines_with_sep += 1;
            }
        }

        if lines_with_sep == base.len() {
            BlockKind::Nested
        } else if lines_with_sep == 0 {
            BlockKind::Text
        } else {
            BlockKind::Mixed
        }
    }

    fn process_value(&mut self, value: &str, depth: usize) -> Result<Value, KvlError> {
        if is_blank(value) {
            return Ok(Value::empty());
        }
        match self.classify_multiline_block(value) {
            BlockKind::Scalar | BlockKind::Text => Ok(leaf(value)),
            BlockKind::Nested => {
                let nested = self.parse_kvs(value, true, depth + 1)?;
                self.build_model(nested, depth + 1)
            }
            BlockKind::Mixed => {
                self.emit_diagnostic(
                    "W002",
                    "Mixed continuation content: some base-level lines have separators \
                     but not all; block treated as plain text",
                    None,
                )?;
                Ok(leaf(value))
            }
        }
    }

    fn build_model(&mut self, key_vals: KeyVals, depth: usize) -> Result<Value, KvlError> {
        // Anonymous-only groups form a list.
        if !key_vals.anons.is_empty() && key_vals.kvs.is_empty() {
            let mut items = Vec::new();
            for anon in key_vals.anons {
                match anon {
                    Anon::Item(content) => items.push(self.process_value(&content, 0)?),
                    Anon::Dict(key, value) => {
                        let single = KeyVals {
                            kvs: vec![(key, value)],
                            anons: Vec::new(),
                        };
                        items.push(self.build_model(single, 0)?);
                    }
                }
            }
            return Ok(Value::List(items));
        }

        let mut groups: indexmap::IndexMap<String, Vec<String>> = indexmap::IndexMap::new();
        for (key, value) in key_vals.kvs {
            groups.entry(key).or_default().push(value);
        }

        let mut result = Map::new();
        for (key, values) in groups {
            let mut merged: Option<Value> = None;
            for value in values {
                let model = self.process_value(&value, depth)?;
                merged = Some(match merged {
                    None => model,
                    Some(acc) => merge(&acc, &model),
                });
            }
            if let Some(m) = merged {
                result.insert(key, m);
            }
        }

        Ok(Value::Map(result))
    }
}

fn leaf(value: &str) -> Value {
    let mut m = Map::new();
    m.insert(value.to_string(), Value::empty());
    Value::Map(m)
}

fn is_list_marker(line: &str, pos: usize, config: &KvlConfig) -> bool {
    if config.list_markers.is_empty() || pos >= line.len() {
        return false;
    }
    let ch = match line[pos..].chars().next() {
        Some(c) => c,
        None => return false,
    };
    if !config.list_markers.contains(ch) {
        return false;
    }
    let suffix = &line[pos + ch.len_utf8()..];
    suffix.is_empty() || suffix.starts_with(' ') || suffix.starts_with('\t')
}

/// Collect a multi-line value; returns the value (with a leading newline) and
/// the index of the first line past the block.
fn collect_multiline_value(
    lines: &[&str],
    start_idx: usize,
    parent_indent: usize,
) -> (String, usize) {
    let mut value_lines: Vec<&str> = Vec::new();
    let mut i = start_idx;
    while i < lines.len() {
        let line = lines[i];
        if is_blank(line) {
            // Peek ahead: include blank lines only if deeper content follows.
            let mut j = i + 1;
            while j < lines.len() && is_blank(lines[j]) {
                j += 1;
            }
            if j < lines.len() {
                let next_indent = line_indent(lines[j]);
                if next_indent > parent_indent {
                    value_lines.extend_from_slice(&lines[i..j]);
                    i = j;
                    continue;
                }
            }
            break;
        }
        if line_indent(line) <= parent_indent {
            break;
        }
        value_lines.push(line);
        i += 1;
    }
    let value = if value_lines.is_empty() {
        String::new()
    } else {
        format!("\n{}", value_lines.join("\n"))
    };
    (value, i)
}

/// Capture a deeper-indented child block if one exists.
fn capture_indented_block(
    lines: &[&str],
    start_idx: usize,
    parent_indent: usize,
) -> (String, usize) {
    if start_idx >= lines.len() {
        return (String::new(), start_idx);
    }
    let mut probe = start_idx;
    while probe < lines.len() && is_blank(lines[probe]) {
        probe += 1;
    }
    if probe >= lines.len() || line_indent(lines[probe]) <= parent_indent {
        return (String::new(), start_idx);
    }
    collect_multiline_value(lines, start_idx, parent_indent)
}

/// Treat inline list-item content with children as the bare-marker form.
fn desugar_list_item_block(item_content: &str, child_block: &str) -> String {
    let mut child_prefix = "";
    for line in child_block.split('\n') {
        if !is_blank(line) {
            child_prefix = &line[..line_indent(line)];
            break;
        }
    }
    format!("\n{child_prefix}{item_content}{child_block}")
}

/// Only desugar inline list items that already carry a real inline payload.
fn should_desugar_list_item_block(item_content: &str, separator: &str) -> bool {
    match find_unescaped_separator(item_content, separator) {
        None => true,
        Some(sep_pos) => !item_content[sep_pos + separator.len()..].trim().is_empty(),
    }
}

/// Verify that indentation uses only tabs or only spaces, not both. The first
/// indented line determines the mode.
fn check_indent_consistency(text: &str) -> Result<(), KvlError> {
    let mut indent_char: Option<char> = None;
    for (idx, line) in split_lines(text).iter().enumerate() {
        let first = match line.chars().next() {
            Some(c) => c,
            None => continue,
        };
        if first != ' ' && first != '\t' {
            continue;
        }
        let leading: Vec<char> = line
            .chars()
            .take_while(|c| *c == ' ' || *c == '\t')
            .collect();
        if leading.is_empty() {
            continue;
        }
        let expected = *indent_char.get_or_insert(leading[0]);
        for (col, ch) in leading.iter().enumerate() {
            if *ch != expected {
                let expected_name = if expected == '\t' { "tabs" } else { "spaces" };
                let found_name = if *ch == '\t' { "tab" } else { "space" };
                return Err(KvlError::parse(
                    format!("Mixed indentation: expected {expected_name} but found {found_name}"),
                    Some(idx + 1),
                    Some(col + 1),
                ));
            }
        }
    }
    Ok(())
}

/// Recursively unescape all keys in a categorical model.
fn unescape_model(model: &Value, separator: &str) -> Value {
    match model {
        Value::List(items) => Value::List(
            items
                .iter()
                .map(|item| unescape_model(item, separator))
                .collect(),
        ),
        Value::Map(m) => Value::Map(
            m.iter()
                .map(|(k, v)| (unescape_text(k, separator), unescape_model(v, separator)))
                .collect(),
        ),
        Value::String(_) => model.clone(),
    }
}

/// Trim a multiline string value for `loads()` output.
///
/// Empty-key continuations (leading `\n`) are dedented by the minimum indent
/// of their non-blank lines; valued-key continuations keep their inline first
/// line and dedent the rest.
fn trim_multiline(value: &str) -> String {
    if !value.contains('\n') {
        return value.to_string();
    }

    if let Some(rest) = value.strip_prefix('\n') {
        let lines: Vec<&str> = rest.split('\n').collect();
        let non_blank: Vec<&str> = lines.iter().filter(|l| !is_blank(l)).copied().collect();
        if non_blank.is_empty() {
            return rest.to_string();
        }
        let min_indent = non_blank.iter().map(|l| line_indent(l)).min().unwrap();
        if min_indent == 0 {
            return rest.to_string();
        }
        return lines
            .iter()
            .map(|l| l.get(min_indent..).unwrap_or(""))
            .collect::<Vec<_>>()
            .join("\n");
    }

    let lines: Vec<&str> = value.split('\n').collect();
    if lines.len() < 2 {
        return value.to_string();
    }
    let cont_lines = &lines[1..];
    let non_blank: Vec<&str> = cont_lines.iter().filter(|l| !is_blank(l)).copied().collect();
    if non_blank.is_empty() {
        return value.to_string();
    }
    let min_indent = non_blank.iter().map(|l| line_indent(l)).min().unwrap();
    if min_indent == 0 {
        return value.to_string();
    }
    let mut dedented = vec![lines[0].to_string()];
    for l in cont_lines {
        dedented.push(l.get(min_indent..).unwrap_or("").to_string());
    }
    dedented.join("\n")
}

fn trim_multiline_values(data: &Value) -> Value {
    match data {
        Value::Map(m) => Value::Map(
            m.iter()
                .map(|(k, v)| (k.clone(), trim_multiline_values(v)))
                .collect(),
        ),
        Value::List(items) => Value::List(items.iter().map(trim_multiline_values).collect()),
        Value::String(s) => Value::String(trim_multiline(s)),
    }
}
