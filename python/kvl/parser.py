"""Parser for Key-Value Language (KVL).

KVL extends the Categorical Configuration Language (CCL) with:
- Configurable separators (=, :, ->, etc.)
- Escape sequences for separators (\\= → =, \\: → :, etc.)
- Optional list compaction for repeated keys
- Schema validation support

Based on the original CCL by Dmitrii Kovanikov (chshersh).
See: https://c-cube.github.io/ocaml-ccl/ and https://github.com/c-cube/ocaml-ccl
"""

from typing import Dict, List, Optional, Tuple, Any
from kvl.errors import KvlParseError
from kvl.config import KvlConfig, parse_header, extract_content
from kvl.utils import handle_read, FileOrPath, ensure_config
from kvl.transform import merge, compact

# Security limits to prevent DoS attacks
MAX_RECURSION_DEPTH = 100  # Maximum nesting depth for structures
MAX_INPUT_SIZE = 10 * 1024 * 1024  # 10MB maximum input size


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

    return compact(raw_model, config)


def load(
    file_or_path: FileOrPath, config: Optional[KvlConfig] = None
) -> Dict[str, Any]:
    """Parse KVL from a file or file path."""
    text = handle_read(file_or_path)
    return loads(text, config)


def _collect_multiline_value(
    lines: List[str], start_idx: int, parent_indent: int
) -> Tuple[str, int]:
    """Collect multi-line value and return (value, new_index)."""
    value_lines = []
    i = start_idx
    while i < len(lines):
        line = lines[i]
        if not line.strip():
            value_lines.append(line)
            i += 1
            continue
        if len(line) - len(line.lstrip()) <= parent_indent:
            break
        value_lines.append(line)
        i += 1
    return ("\n" + "\n".join(value_lines) if value_lines else "", i)


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

            # Check if this list item contains a separator (is a key-value pair)
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
                    if not item_value and i + 1 < len(lines):
                        next_line = lines[i + 1] if i + 1 < len(lines) else ""
                        if next_line and (
                            next_line.startswith(" ") or next_line.startswith("\t")
                        ):
                            parent_indent = len(line) - len(line.lstrip())
                            item_value, new_i = _collect_multiline_value(
                                lines, i + 1, parent_indent
                            )
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
        line_indent = len(line) - len(line.lstrip())
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
            # No separator found - treat whole line as key with empty value
            key = line.strip()
            value_part = ""
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
        if not value_part and i + 1 < len(lines):
            next_line = lines[i + 1] if i + 1 < len(lines) else ""
            if next_line and (next_line.startswith(" ") or next_line.startswith("\t")):
                parent_indent = len(line) - len(line.lstrip())
                value_part, new_i = _collect_multiline_value(
                    lines, i + 1, parent_indent
                )
                i = new_i - 1  # Adjust since main loop will increment

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
    return (
        line[pos] in config.list_markers
        and pos + 1 < len(line)
        and line[pos + 1] in " \t"
    )


def _find_unescaped_separator(line: str, separator: str) -> int:
    """Find the first separator not preceded by backslashes.

    KVL extends CCL with escape sequences: backslash + separator escapes the separator.
    This allows values to contain separator characters.

    Examples:
        "url = https\\://example.com" → key="url", value="https://example.com"
        "equation = x\\=y+z" → key="equation", value="x=y+z"
    """
    pos = 0
    while pos < len(line):
        sep_pos = line.find(separator, pos)
        if sep_pos == -1:
            return -1
        if sep_pos > 0 and line[sep_pos - 1] == "\\":
            pos = sep_pos + 1
        else:
            return sep_pos
    return -1


def _process_value(value: str, config: KvlConfig, depth: int = 0) -> Dict[str, Any]:
    """Process a single value into a categorical model."""
    if not value or value.strip() == "":
        return {}

    # Only parse as nested KVL if it contains separators or list markers
    # Otherwise treat as literal value
    has_separator = config.separator in value
    has_list_markers = config.list_markers and any(
        any(
            line.lstrip().startswith(marker + " ")
            or line.lstrip().startswith(marker + "\t")
            for marker in config.list_markers
        )
        for line in value.split("\n")
        if line.strip()
    )
    if not has_separator and not has_list_markers:
        # Return as categorical leaf: {value: {}}
        return {value: {}}

    # Processing nested values, allow anonymous lists
    nested_kvs = _parse_kvs(value, config, allow_anonymous_lists=True, depth=depth + 1)
    return _build_model(nested_kvs, config, depth)


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
    
    Simple replacement that handles:
    - \\<sep> → <sep> (escaped separator becomes literal separator)
    - \\\\<sep> → \\<sep> (double backslash before sep becomes single backslash + sep)
    - \\\\\\<sep> → \\\\<sep> (triple backslash becomes double backslash + sep)
    
    Examples:
        "x\\=y" with separator "=" → "x=y"
        "x\\\\=y" with separator "=" → "x\\=y" 
        "x\\\\\\=y" with separator "=" → "x\\\\=y"
    """
    escaped_sep = "\\" + separator
    return text.replace(escaped_sep, separator)
