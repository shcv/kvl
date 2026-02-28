"""Serializer for Key-Value Language (KVL).

Based on the original Categorical Configuration Language (CCL) by Dmitrii Kovanikov (chshersh).
See: https://c-cube.github.io/ocaml-ccl/ and https://github.com/c-cube/ocaml-ccl
"""

from typing import Dict, List, Union, Optional
from kvl.errors import KvlSerializeError
from kvl.utils import KvlValue, handle_write, FileOrPath, ensure_config
from kvl.config import KvlConfig, generate_header
from kvl.transform import expand


def _determine_list_marker(list_marker: Optional[str], config: KvlConfig) -> Optional[str]:
    """
    Determine which list marker to use for serialization.
    
    Args:
        list_marker: Explicitly provided list marker (None means auto-detect)
        config: KVL configuration
        
    Returns:
        List marker to use, or None for categorical format
    """
    if list_marker is not None:
        # Explicit override - empty string means no list markers
        return list_marker if list_marker else None
    
    # Auto-detect from config
    if config.list_markers:
        # Use the first list marker character
        return config.list_markers[0]
    
    # No list markers configured
    return None


def dumps(data: Dict[str, KvlValue], config: Optional[KvlConfig] = None, include_header: bool = False, 
          list_marker: Optional[str] = None, **options) -> str:
    """
    Serialize data to KVL format.

    Args:
        data: Dictionary containing data to serialize.
        config: KVL configuration with separator info.
        include_header: Whether to include KVL header line.
        list_marker: List marker to use for serializing lists. If None, auto-detects from config.list_markers.
                    Set to empty string to force categorical format.
        options: Optional serialization parameters.

    Returns:
        String containing serialized KVL data.

    Raises:
        KvlSerializeError: If the data cannot be serialized.
    """
    config = ensure_config(config, KvlConfig)
        
    if not data:
        return generate_header(config) + "\n" if include_header else ""

    # Determine which list marker to use
    effective_list_marker = _determine_list_marker(list_marker, config)
    
    # If we have a list marker, serialize with list marker format
    # Otherwise, convert lists to categorical format
    if effective_list_marker:
        serializer = KvlSerializer(data, config=config, list_marker=effective_list_marker, **options)
    else:
        # Convert lists back to KVL dict format before serialization (categorical format)
        kvl_data = expand(data)
        serializer = KvlSerializer(kvl_data, config=config, **options)
    
    result = serializer.serialize()
    
    if include_header:
        result = generate_header(config) + "\n" + result
        
    return result


def dump(
    data: Dict[str, KvlValue], file_or_path: FileOrPath, config: Optional[KvlConfig] = None, include_header: bool = False, 
    list_marker: Optional[str] = None, **options
) -> None:
    """
    Serialize data to KVL format and write to a file.

    Args:
        data: Dictionary containing data to serialize.
        file_or_path: File-like object or path to write to.
        config: KVL configuration with separator info.
        include_header: Whether to include KVL header line.
        list_marker: List marker to use for serializing lists. If None, auto-detects from config.list_markers.
        options: Optional serialization parameters.

    Raises:
        KvlSerializeError: If the data cannot be serialized.
        IOError: If the file cannot be written.
    """
    serialized = dumps(data, config=config, include_header=include_header, list_marker=list_marker, **options)
    handle_write(file_or_path, serialized)


class KvlSerializer:
    """Serializer for KVL documents."""

    def __init__(self, data: Dict[str, KvlValue], config: Optional[KvlConfig] = None, indent: str = "  ", list_marker: Optional[str] = None):
        """
        Initialize the serializer with data to serialize.

        Args:
            data: Dictionary containing data to serialize.
            config: KVL configuration with separator info.
            indent: String to use for indentation (default 2 spaces to match OCaml).
            list_marker: List marker character to use for serializing lists (e.g., '-').
        """
        self.data = data
        self.config = config if config is not None else KvlConfig()
        self.indent = indent
        self.list_marker = list_marker
        self.lines: List[str] = []

    def serialize(self) -> str:
        """
        Serialize the data to KVL format.

        Returns:
            String containing serialized KVL data.

        Raises:
            KvlSerializeError: If the data cannot be serialized.
        """
        if not self.data:
            return ""

        self._serialize_dict(self.data, 0)
        result = "\n".join(self.lines)

        # Add final newline if there's content
        if result:
            result += "\n"

        return result

    def _escape_text(self, text: str) -> str:
        """Escape separator occurrences within text."""
        return _escape_separator(text, self.config.separator)

    def _format_separator(self, for_empty: bool = False, for_multiline: bool = False) -> str:
        """Format separator with appropriate spacing."""
        before = " " if self.config.space_before else ""
        
        if for_empty:
            # For empty values or nested records, no space after separator
            after = ""
        elif for_multiline:
            # For multiline values, no space after separator  
            after = ""
        else:
            # For single-line values, use configured spacing
            after = " " if self.config.space_after else ""
            
        return f"{before}{self.config.separator}{after}"

    def _serialize_dict(self, data: Dict[str, KvlValue], level: int) -> None:
        """
        Serialize a dictionary to KVL format.

        Args:
            data: Dictionary to serialize.
            level: Current indentation level.

        Raises:
            KvlSerializeError: If the data cannot be serialized.
        """
        indent_str = self.indent * level

        for key, value in data.items():
            if not isinstance(key, str):
                raise KvlSerializeError(
                    f"Keys must be strings, got {type(key).__name__}"
                )

            if isinstance(value, dict):
                self._serialize_dict_value(key, value, indent_str, level)
            elif isinstance(value, list):
                self._serialize_list_value(key, value, indent_str, level)
            elif isinstance(value, str):
                self._serialize_string_value(key, value, indent_str)
            else:
                raise KvlSerializeError(
                    f"Unsupported value type: {type(value).__name__}"
                )

    def _serialize_dict_value(self, key: str, value: dict, indent_str: str, level: int) -> None:
        """Serialize a dictionary value."""
        sep_str = self._format_separator(for_empty=True)
        escaped_key = self._escape_text(key)
        self.lines.append(f"{indent_str}{escaped_key}{sep_str}")
        if value:  # Non-empty dict - serialize nested content
            self._serialize_dict(value, level + 1)
    
    def _serialize_list_value(self, key: str, value: list, indent_str: str, level: int) -> None:
        """Serialize a list value."""
        if not self.list_marker:
            raise KvlSerializeError(
                f"List values are not supported without list markers. "
                f"Use expand() to convert to categorical format."
            )
        sep_str = self._format_separator(for_empty=True)
        escaped_key = self._escape_text(key)
        self.lines.append(f"{indent_str}{escaped_key}{sep_str}")
        self._serialize_list(value, level + 1)
    
    def _serialize_string_value(self, key: str, value: str, indent_str: str) -> None:
        """Serialize a string value."""
        escaped_key = self._escape_text(key)
        escaped_value = self._escape_text(value)
        if not value:
            sep_str = self._format_separator(for_empty=True)
            self.lines.append(f"{indent_str}{escaped_key}{sep_str}")
        elif "\n" in value:
            sep_str = self._format_separator(for_multiline=True)
            self.lines.append(f"{indent_str}{escaped_key}{sep_str}{escaped_value}")
        else:
            sep_str = self._format_separator()
            self.lines.append(f"{indent_str}{escaped_key}{sep_str}{escaped_value}")

    def _serialize_list(self, items: List[KvlValue], level: int) -> None:
        """
        Serialize a list using list markers.
        
        Args:
            items: List of items to serialize.
            level: Current indentation level.
        """
        indent_str = self.indent * level
        marker_str = f"{self.list_marker} "
        
        for item in items:
            if isinstance(item, str):
                # Simple string item
                escaped_item = self._escape_text(item)
                self.lines.append(f"{indent_str}{marker_str}{escaped_item}")
            elif isinstance(item, dict):
                # Complex dict item - serialize as nested structure under list marker
                if len(item) == 1:
                    # Single key-value pair - can put on same line
                    key, value = next(iter(item.items()))
                    if isinstance(value, str) and "\n" not in value:
                        # Simple key-value pair
                        sep_str = self._format_separator()
                        escaped_key = self._escape_text(key)
                        escaped_value = self._escape_text(value)
                        self.lines.append(f"{indent_str}{marker_str}{escaped_key}{sep_str}{escaped_value}")
                    else:
                        # Complex value - needs nested structure
                        sep_str = self._format_separator(for_empty=True)
                        escaped_key = self._escape_text(key)
                        self.lines.append(f"{indent_str}{marker_str}{escaped_key}{sep_str}")
                        if isinstance(value, dict):
                            self._serialize_dict(value, level + 1)
                        elif isinstance(value, list) and self.list_marker:
                            self._serialize_list(value, level + 1)
                        else:
                            # Multi-line string or other complex value
                            self.lines.append(f"{self.indent * (level + 1)}{value}")
                else:
                    # Multiple key-value pairs - each gets its own nested line
                    first_key = True
                    for key, value in item.items():
                        if first_key:
                            # First key uses the list marker
                            sep_str = self._format_separator() if isinstance(value, str) and "\n" not in value else self._format_separator(for_empty=True)
                            if isinstance(value, str) and "\n" not in value:
                                escaped_key = self._escape_text(key)
                                escaped_value = self._escape_text(value)
                                self.lines.append(f"{indent_str}{marker_str}{escaped_key}{sep_str}{escaped_value}")
                            else:
                                escaped_key = self._escape_text(key)
                                self.lines.append(f"{indent_str}{marker_str}{escaped_key}{sep_str}")
                                if isinstance(value, dict):
                                    self._serialize_dict(value, level + 1)
                                elif isinstance(value, list) and self.list_marker:
                                    self._serialize_list(value, level + 1)
                                else:
                                    self.lines.append(f"{self.indent * (level + 1)}{value}")
                            first_key = False
                        else:
                            # Subsequent keys are indented normally
                            nested_indent = self.indent * (level + 1)
                            sep_str = self._format_separator() if isinstance(value, str) and "\n" not in value else self._format_separator(for_empty=True)
                            if isinstance(value, str) and "\n" not in value:
                                escaped_key = self._escape_text(key)
                                escaped_value = self._escape_text(value)
                                self.lines.append(f"{nested_indent}{escaped_key}{sep_str}{escaped_value}")
                            else:
                                escaped_key = self._escape_text(key)
                                self.lines.append(f"{nested_indent}{escaped_key}{sep_str}")
                                if isinstance(value, dict):
                                    self._serialize_dict(value, level + 2)
                                elif isinstance(value, list) and self.list_marker:
                                    self._serialize_list(value, level + 2)
                                else:
                                    self.lines.append(f"{self.indent * (level + 2)}{value}")
            else:
                # Other types - convert to string
                self.lines.append(f"{indent_str}{marker_str}{str(item)}")

    def _format_value(self, value: KvlValue) -> str:
        """
        Format a value for KVL serialization.

        Args:
            value: Value to format.

        Returns:
            Formatted string representation of the value.

        Raises:
            KvlSerializeError: If the value cannot be formatted.
        """
        if isinstance(value, str):
            return value
        elif isinstance(value, dict):
            raise KvlSerializeError("Nested dicts should be handled by _serialize_dict")
        else:
            raise KvlSerializeError(f"Unsupported value type: {type(value).__name__}")


def _escape_separator(text: str, separator: str) -> str:
    """Escape every occurrence of the current separator."""
    if not text or not separator:
        return text
    return text.replace(separator, "\\" + separator)
