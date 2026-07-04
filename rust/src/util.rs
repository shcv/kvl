//! Small text helpers shared by the parser and serializer.

/// Split text into lines like Python's `str.splitlines()` for `\n`/`\r\n`
/// input: no trailing empty line for text ending in a newline.
pub fn split_lines(text: &str) -> Vec<&str> {
    if text.is_empty() {
        return Vec::new();
    }
    let mut lines: Vec<&str> = text.split('\n').collect();
    if lines.last() == Some(&"") {
        lines.pop();
    }
    lines
        .into_iter()
        .map(|l| l.strip_suffix('\r').unwrap_or(l))
        .collect()
}

/// Byte width of a line's leading whitespace.
pub fn line_indent(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

pub fn is_blank(line: &str) -> bool {
    line.trim().is_empty()
}

/// Find the first occurrence of `separator` not immediately preceded by a
/// backslash. Simple escape rule: `\` directly before the separator escapes
/// it; no pair processing.
pub fn find_unescaped_separator(line: &str, separator: &str) -> Option<usize> {
    let sep_len = separator.len();
    if sep_len == 0 {
        return None;
    }
    let mut pos = 0;
    while pos + sep_len <= line.len() {
        let off = line[pos..].find(separator)?;
        let sep_pos = pos + off;
        if sep_pos > 0 && line.as_bytes()[sep_pos - 1] == b'\\' {
            let first_char_len = separator.chars().next().map(|c| c.len_utf8()).unwrap_or(1);
            pos = sep_pos + first_char_len;
            continue;
        }
        return Some(sep_pos);
    }
    None
}

/// Unescape separator patterns: `\` + separator becomes the bare separator.
/// All other backslashes are literal.
pub fn unescape_text(text: &str, separator: &str) -> String {
    let escaped = format!("\\{separator}");
    if !text.contains(&escaped) {
        return text.to_string();
    }
    let sep_len = separator.len();
    let mut result = String::with_capacity(text.len());
    let mut i = 0;
    let bytes = text.as_bytes();
    while i < text.len() {
        if i + sep_len < text.len()
            && bytes[i] == b'\\'
            && &bytes[i + 1..i + 1 + sep_len] == separator.as_bytes()
        {
            result.push_str(separator);
            i += 1 + sep_len;
        } else {
            let ch = text[i..].chars().next().unwrap();
            result.push(ch);
            i += ch.len_utf8();
        }
    }
    result
}

/// Escape every occurrence of the separator with a backslash.
pub fn escape_separator(text: &str, separator: &str) -> String {
    if text.is_empty() || separator.is_empty() {
        return text.to_string();
    }
    text.replace(separator, &format!("\\{separator}"))
}
