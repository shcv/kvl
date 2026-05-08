"""Parser for Key-Value Language (KVL).

KVL extends the Categorical Configuration Language (CCL) with:
- Configurable separators (=, :, ->, etc.)
- Escape sequences for separators (\\= → =, \\: → :, etc.)
- Optional list compaction for repeated keys
- Schema validation support

Based on the original CCL by Dmitrii Kovanikov (chshersh).
See: https://github.com/chshersh/ccl
"""

from typing import Dict, List, Optional, Tuple, Any
from kvl.errors import KvlParseError, KvlDiagnostic
from kvl.config import KvlConfig, parse_header, extract_content
from kvl.utils import handle_read, FileOrPath, ensure_config
from kvl.transform import merge, compact

# Security limits to prevent DoS attacks
MAX_RECURSION_DEPTH = 100  # Maximum nesting depth for structures
MAX_INPUT_SIZE = 10 * 1024 * 1024  # 10MB maximum input size


def _emit_diagnostic(config: KvlConfig, code: str, message: str, line: Optional[int] = None) -> None:
    """Emit a diagnostic (warning or error in strict mode)."""
    if config.strict:
        raise KvlParseError(f"[{code}] {message}", line=line)
    config.diagnostics.append(KvlDiagnostic(
        severity="warning", code=code, message=message, line=line
    ))


def keyvals(text: str, config: Optional[KvlConfig] = None) -> List[Dict[str, str]]:
    """Parse KVL text into a list of single-key dictionaries."""
    config = ensure_config(config, lambda: parse_header(text) or KvlConfig())
    return _parse_kvs(text, config) if text.strip() else []


def parse(text: str, config: Optional[KvlConfig] = None) -> Dict[str, Any]:
    """Parse KVL text into raw categorical model (low-level API).

    This is the low-level API that returns the raw categorical structure where
    repeated keys are represented as nested dictionaries. For the standard
    user-facing API that automatically compacts lists, use loads() instead.

    Args:
        text: KVL text to parse
        config: Optional KVL configuration

    Returns:
        Parsed categorical model as nested dictionaries. Repeated keys appear
        as nested mappings: {'tags': {'web': {}, 'api': {}, 'prod': {}}}

    Raises:
        KvlParseError: If input is too large or parsing fails

    Example:
        >>> data = parse("tags = web\\ntags = api")
        >>> data
        {'tags': {'web': {}, 'api': {}}}

    See Also:
        loads(): High-level API that automatically compacts to lists
        compact(): Convert categorical structure to compacted form
    """
    if len(text) > MAX_INPUT_SIZE:
        raise KvlParseError(f"Input exceeds maximum size of {MAX_INPUT_SIZE} bytes")
    _check_indent_consistency(text)
    config = ensure_config(config, lambda: parse_header(text) or KvlConfig())
    key_vals = keyvals(text, config)
    model = _build_model(key_vals, config)
    return _unescape_model(model, config)


def loads(text: str, config: Optional[KvlConfig] = None) -> Dict[str, Any]:
    """Parse KVL text into user-friendly compacted data (standard API).

    This is the standard high-level API that automatically compacts categorical
    structures into lists, providing the intuitive data format most users expect.

    Args:
        text: KVL text to parse
        config: Optional KVL configuration

    Returns:
        Parsed and compacted data structure. Repeated keys are automatically
        converted to lists: {'tags': ['web', 'api', 'prod']}

    Raises:
        KvlParseError: If input is too large or parsing fails

    Example:
        >>> data = loads("tags = web\\ntags = api")
        >>> data
        {'tags': ['web', 'api']}

    See Also:
        parse(): Low-level API that returns raw categorical structure
        load(): Load and parse KVL from a file
    """
    config = ensure_config(config, lambda: parse_header(text) or KvlConfig())
    raw_model = parse(text, config)
    compacted = compact(raw_model, config)

    return _trim_multiline_values(compacted)


def load(
    file_or_path: FileOrPath, config: Optional[KvlConfig] = None
) -> Dict[str, Any]:
    """Parse KVL from a file or file path."""
    text = handle_read(file_or_path)
    return loads(text, config)


def _trim_multiline(value: str) -> str:
    """Trim a multiline string value for loads() output.

    Two cases:
    - Starts with \\n (empty-key continuation): strip leading \\n, dedent all lines
    - Does not start with \\n (valued-key continuation): first line is inline,
      dedent only subsequent lines by their min indent
    """
    if '\n' not in value:
        return value

    if value.startswith('\n'):
        # Empty-key continuation: strip leading \n, dedent all lines
        value = value[1:]
        lines = value.split('\n')
        non_blank = [l for l in lines if l.strip()]
        if not non_blank:
            return value
        min_indent = min(len(l) - len(l.lstrip()) for l in non_blank)
        if min_indent == 0:
            return value
        return '\n'.join(l[min_indent:] for l in lines)

    # Valued-key continuation: first line is inline value, rest are continuation
    lines = value.split('\n')
    if len(lines) < 2:
        return value
    cont_lines = lines[1:]
    non_blank_cont = [l for l in cont_lines if l.strip()]
    if not non_blank_cont:
        return value
    min_indent = min(len(l) - len(l.lstrip()) for l in non_blank_cont)
    if min_indent == 0:
        return value
    dedented = [lines[0]]
    for l in cont_lines:
        dedented.append(l[min_indent:])
    return '\n'.join(dedented)


def _trim_multiline_values(data: Any) -> Any:
    """Walk a compacted data structure and trim multiline string values."""
    if isinstance(data, dict):
        return {k: _trim_multiline_values(v) for k, v in data.items()}
    elif isinstance(data, list):
        return [_trim_multiline_values(item) for item in data]
    elif isinstance(data, str):
        return _trim_multiline(data)
    return data


def _check_indent_consistency(text: str) -> None:
    """Verify that indentation uses only tabs or only spaces, not both.

    The first indented line determines the indent mode. Subsequent indented
    lines must use the same character. Raises KvlParseError if mixed.
    """
    indent_char = None
    for line_num, line in enumerate(text.splitlines(), 1):
        if not line or not line[0] in (' ', '\t'):
            continue
        # Extract leading whitespace
        leading = ""
        for ch in line:
            if ch in (' ', '\t'):
                leading += ch
            else:
                break
        if not leading:
            continue
        if indent_char is None:
            indent_char = leading[0]
        for col, ch in enumerate(leading):
            if ch != indent_char:
                expected = "tabs" if indent_char == '\t' else "spaces"
                found = "tab" if ch == '\t' else "space"
                raise KvlParseError(
                    f"Mixed indentation: expected {expected} but found {found}",
                    line=line_num, column=col + 1
                )


def _collect_multiline_value(
    lines: List[str], start_idx: int, parent_indent: int
) -> Tuple[str, int]:
    """Collect multi-line value and return (value, new_index)."""
    value_lines = []
    i = start_idx
    while i < len(lines):
        line = lines[i]
        if not line.strip():
            # Peek ahead: only include blank lines if more indented content follows
            j = i + 1
            while j < len(lines) and not lines[j].strip():
                j += 1
            if j < len(lines):
                next_indent = len(lines[j]) - len(lines[j].lstrip())
                if next_indent > parent_indent:
                    for k in range(i, j):
                        value_lines.append(lines[k])
                    i = j
                    continue
            break
        if len(line) - len(line.lstrip()) <= parent_indent:
            break
        value_lines.append(line)
        i += 1
    return ("\n" + "\n".join(value_lines) if value_lines else "", i)


def _line_indent(line: str) -> int:
    """Return the leading indentation width of a line."""
    return len(line) - len(line.lstrip())


def _capture_indented_block(
    lines: List[str], start_idx: int, parent_indent: int
) -> Tuple[str, int]:
    """Capture a deeper-indented child block if one exists."""
    if start_idx >= len(lines):
        return "", start_idx

    probe_idx = start_idx
    while probe_idx < len(lines) and not lines[probe_idx].strip():
        probe_idx += 1

    if probe_idx >= len(lines) or _line_indent(lines[probe_idx]) <= parent_indent:
        return "", start_idx

    return _collect_multiline_value(lines, start_idx, parent_indent)


def _classify_multiline_block(value: str, config: KvlConfig) -> str:
    """Classify a captured multiline block as nested KVL, plain text, or mixed."""
    if '\n' not in value:
        return "scalar"

    lines = value.split("\n")
    non_blank_lines = [line for line in lines if line.strip()]
    if not non_blank_lines:
        return "text"

    min_indent = min(_line_indent(line) for line in non_blank_lines)
    base_lines = [line for line in non_blank_lines if _line_indent(line) == min_indent]

    lines_with_sep = 0
    for line in base_lines:
        content = line.lstrip()
        has_sep = _find_unescaped_separator(content, config.separator) != -1
        has_marker = bool(config.list_markers and _is_list_marker(content, 0, config))
        if has_sep or has_marker:
            lines_with_sep += 1

    if lines_with_sep == len(base_lines):
        return "nested"
    if lines_with_sep == 0:
        return "text"
    return "mixed"


def _desugar_list_item_block(item_content: str, child_block: str) -> str:
    """Treat inline list-item content with children as the bare-marker form."""
    block_lines = child_block.split("\n")
    child_prefix = ""
    for line in block_lines:
        if line.strip():
            child_prefix = line[:_line_indent(line)]
            break
    return "\n" + child_prefix + item_content + child_block


def _should_desugar_list_item_block(item_content: str, separator: str) -> bool:
    """Only desugar inline list items that already carry a real inline payload."""
    sep_pos = _find_unescaped_separator(item_content, separator)
    if sep_pos == -1:
        return True
    return bool(item_content[sep_pos + len(separator):].strip())


def _parse_kvs(
    text: str, config: KvlConfig, allow_anonymous_lists: bool = False, depth: int = 0
) -> List[Dict[str, str]]:
    """Parse key-value pairs.

    Args:
        text: The text to parse
        config: KVL configuration
        allow_anonymous_lists: If True, list items without a key form an anonymous list
        depth: Current recursion depth for security limits

    Raises:
        KvlParseError: If maximum recursion depth is exceeded
    """
    if depth > MAX_RECURSION_DEPTH:
        raise KvlParseError(f"Maximum nesting depth of {MAX_RECURSION_DEPTH} exceeded")
    # Extract content without header
    content = extract_content(text)
    lines = content.splitlines()
    result = []
    i = 0
    current_list_key = None  # Track the current list we're building
    anonymous_list_items = []  # Track anonymous list items

    while i < len(lines):
        line = lines[i]
        if not line.strip():
            i += 1
            continue

        # Get the first non-whitespace position
        first_char_pos = len(line) - len(line.lstrip())
        if first_char_pos < len(line) and _is_list_marker(line, first_char_pos, config):
            # This is a list item
            # Remove the marker and following whitespace from that position
            list_item_content = line[first_char_pos + 1 :].lstrip()
            parent_indent = _line_indent(line)
            child_block, new_i = _capture_indented_block(lines, i + 1, parent_indent)
            if child_block:
                if not list_item_content:
                    list_item_content = child_block
                    i = new_i - 1
                elif _should_desugar_list_item_block(
                    list_item_content, config.separator
                ):
                    list_item_content = _desugar_list_item_block(
                        list_item_content, child_block
                    )
                    i = new_i - 1
                elif current_list_key:
                    list_item_content = list_item_content + child_block
                    i = new_i - 1

            # Check if this list item contains a separator (is a key-value pair)
            item_sep_pos = -1
            if not list_item_content.startswith("\n"):
                item_sep_pos = _find_unescaped_separator(
                    list_item_content, config.separator
                )

            if current_list_key:
                # Continue the current list
                result.append({current_list_key: list_item_content})
            elif allow_anonymous_lists:
                if item_sep_pos != -1:
                    # This list item is a key-value pair
                    item_key = list_item_content[:item_sep_pos].strip()
                    item_value = list_item_content[
                        item_sep_pos + len(config.separator) :
                    ].strip()

                    # Handle multi-line values for list item
                    if not item_value and child_block:
                        item_value = child_block
                        i = new_i - 1  # Adjust since main loop will increment
                    elif not item_value:
                        item_value, new_i = _capture_indented_block(
                            lines, i + 1, parent_indent
                        )
                        if item_value:
                            i = new_i - 1  # Adjust since main loop will increment

                    # Add as anonymous key-value pair
                    anonymous_list_items.append({item_key: item_value})
                else:
                    # Simple list item
                    anonymous_list_items.append(list_item_content)
            else:
                # Error: list item without a key
                raise KvlParseError(
                    f"List item found without preceding key at line {i+1}"
                )

            i += 1
            continue

        # Check for indented line without separator (multiline continuation)
        line_indent = _line_indent(line)
        sep_pos = _find_unescaped_separator(line, config.separator)

        if sep_pos == -1 and line_indent > 0 and result:
            # Indented line with no separator - append to previous value
            last_entry = result[-1]
            last_key = list(last_entry.keys())[0]
            if last_entry[last_key]:
                last_entry[last_key] += "\n" + line.rstrip()
            else:
                last_entry[last_key] = "\n" + line.rstrip()
            i += 1
            continue
        elif sep_pos == -1:
            # No separator found - parse error (separator is mandatory)
            raise KvlParseError(
                f"Missing separator '{config.separator}' on line",
                line=i + 1,
            )
        else:
            key = line[:sep_pos].strip()
            value_part = line[sep_pos + len(config.separator) :].strip()

        # Only set current_list_key if this key has an empty value (will have nested content)
        # List items should only attach to keys that have nested content, not all keys
        if not value_part:
            current_list_key = key
        else:
            # This key has a value, so subsequent list items should not attach to it
            current_list_key = None

        # Handle multi-line values
        parent_indent = _line_indent(line)
        if not value_part:
            value_part, new_i = _capture_indented_block(
                lines, i + 1, parent_indent
            )
            if value_part:
                i = new_i - 1  # Adjust since main loop will increment
        else:
            # Valued key with indented children → continuation text (W001)
            cont_value, new_i = _capture_indented_block(
                lines, i + 1, parent_indent
            )
            if cont_value:
                _emit_diagnostic(config, "W001",
                    "Valued key with continuation: indented lines after "
                    "a valued key are treated as continuation text",
                    line=i + 1)
                value_part = value_part + cont_value
                i = new_i - 1

        result.append({key: value_part})
        i += 1

    # Append anonymous list items as tagged tuples
    if anonymous_list_items and allow_anonymous_lists:
        for item in anonymous_list_items:
            if isinstance(item, dict):
                result.append(("anon_dict", item))
            else:
                result.append(("anon_list", item))

    return result


def _is_list_marker(line: str, pos: int, config: KvlConfig) -> bool:
    """Check if position contains a valid list marker."""
    if not config.list_markers or pos >= len(line):
        return False
    suffix = line[pos + 1:]
    return (
        line[pos] in config.list_markers
        and (not suffix or suffix[0] in " \t")
    )


def _find_unescaped_separator(line: str, separator: str) -> int:
    """Find the first separator not immediately preceded by a backslash.

    Simple escape rule: a backslash directly before the separator escapes it.
    No pair processing — ``\\=`` means the second backslash escapes the ``=``,
    regardless of how many backslashes precede it.
    """
    sep_len = len(separator)
    pos = 0
    while pos <= len(line) - sep_len:
        sep_pos = line.find(separator, pos)
        if sep_pos == -1:
            return -1
        if sep_pos > 0 and line[sep_pos - 1] == '\\':
            # Backslash immediately before separator = escaped
            pos = sep_pos + 1
            continue
        return sep_pos
    return -1


def _process_value(value: str, config: KvlConfig, depth: int = 0) -> Dict[str, Any]:
    """Process a single value into a categorical model."""
    if not value or value.strip() == "":
        return {}

    # Only re-parse as nested KVL if the value is multiline (from indented
    # blocks).  Single-line values are always literal leaves — they must NOT
    # be re-split on the separator.
    block_kind = _classify_multiline_block(value, config)

    if block_kind == "scalar":
        return {value: {}}

    if block_kind == "nested":
        nested_kvs = _parse_kvs(value, config, allow_anonymous_lists=True, depth=depth + 1)
        return _build_model(nested_kvs, config, depth + 1)

    if block_kind == "text":
        return {value: {}}

    _emit_diagnostic(config, "W002",
        "Mixed continuation content: some base-level lines have separators "
        "but not all; block treated as plain text")
    return {value: {}}


def _build_model(
    key_vals: List, config: KvlConfig, depth: int = 0
) -> Dict[str, Any]:
    """Convert list of key-value pairs to categorical model."""
    anonymous = []
    regular_kvs = []

    for kv in key_vals:
        if isinstance(kv, tuple):
            anonymous.append(kv)
        else:
            regular_kvs.append(kv)

    # If we only have anonymous items, return them as a list
    if anonymous and not regular_kvs:
        processed_items = []
        for tag, item in anonymous:
            if tag == "anon_list":
                processed_items.append(_process_value(item, config))
            else:
                processed_items.append(_build_model([item], config))
        return processed_items

    # Group by key and process values
    key_groups = {}
    for kv in regular_kvs:
        for key, value in kv.items():
            key_groups.setdefault(key, []).append(value)

    # Process and merge values for each key
    result_data = {}
    for key, values in key_groups.items():
        processed = [_process_value(value, config, depth) for value in values]
        if processed:
            merged = processed[0]
            for model in processed[1:]:
                merged = merge(merged, model)
            result_data[key] = merged

    return result_data


def _unescape_model(model, config: KvlConfig):
    """Recursively unescape all keys and values in the model."""
    if isinstance(model, list):
        return [_unescape_model(item, config) for item in model]
    elif isinstance(model, dict):
        return {
            _unescape_text(k, config.separator): _unescape_model(v, config)
            for k, v in model.items()
        }
    else:
        return model


def _unescape_text(text: str, separator: str) -> str:
    """Unescape separator patterns: backslash + separator → separator.

    Scans left-to-right: a backslash immediately followed by the separator
    is consumed as an escape (producing just the separator).  All other
    backslashes are literal.

    Examples:
        "x\\=y"    with sep "=" → "x=y"
        "x\\\\=y"  with sep "=" → "x\\=y"   (first \\ literal, second \\= → =)
        "x\\\\\\=y" with sep "=" → "x\\\\=y"
    """
    escaped = '\\' + separator
    if escaped not in text:
        return text

    result = []
    i = 0
    sep_len = len(separator)
    while i < len(text):
        if (i < len(text) - sep_len
                and text[i] == '\\'
                and text[i + 1:i + 1 + sep_len] == separator):
            result.append(separator)
            i += 1 + sep_len
        else:
            result.append(text[i])
            i += 1
    return ''.join(result)
