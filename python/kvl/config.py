"""Configuration classes for KVL parsing and serialization.

Based on the original Categorical Configuration Language (CCL) by Dmitrii Kovanikov (chshersh).
See: https://c-cube.github.io/ocaml-ccl/ and https://github.com/c-cube/ocaml-ccl
"""

import re
from typing import Optional, Dict, Any
from dataclasses import dataclass

from kvl.errors import KvlParseError


@dataclass
class KvlConfig:
    """Configuration for KVL parsing and serialization."""
    
    separator: str = "="
    version: str = "1.0"
    compact: bool = False
    space_before: bool = True
    space_after: bool = True
    list_markers: str = ""  # Characters that mark list items (e.g., "-+*")
    
    def __post_init__(self):
        """Validate configuration after initialization."""
        if not self.separator:
            raise ValueError("Separator cannot be empty")
        
        # Validate that separator doesn't contain problematic characters
        if any(c in self.separator for c in [' ', '\t', '\n', '\r']):
            raise ValueError("Separator cannot contain whitespace characters")


def auto_config_for_separator(separator: str, **kwargs) -> KvlConfig:
    """Create a KvlConfig with automatically configured spacing for the separator."""
    # Default spacing rules based on separator
    if separator == ":":
        # Colon is unique - no space before, space after (like JSON/YAML)
        defaults = {"space_before": False, "space_after": True}
    else:
        # Most separators follow the pattern: space before and after
        defaults = {"space_before": True, "space_after": True}
    
    # Override defaults with any explicitly provided kwargs
    config_kwargs = {**defaults, "separator": separator, **kwargs}
    return KvlConfig(**config_kwargs)


def parse_header(text: str) -> Optional[KvlConfig]:
    """
    Parse KVL header from the first line of text.
    
    Expected format: #<separator> kvl <version> [list_markers] [options]
    Examples:
        #= kvl 1.0
        #: kvl 1.0 -
        #→ kvl 1.0 -+*
        #:= kvl 1.0 compact
    
    Args:
        text: KVL text that may contain a header
        
    Returns:
        KvlConfig object if header found, None otherwise
        
    Raises:
        KvlParseError: If header format is invalid
    """
    lines = text.splitlines()
    if not lines:
        return None
        
    first_line = lines[0].strip()
    if not first_line.startswith('#'):
        return None
        
    # Extract separator from between # and ' kvl'
    kvl_match = re.match(r'^#(.+?)\s+kvl(?:\s+(.*))?$', first_line)
    if not kvl_match:
        return None
        
    separator = kvl_match.group(1)
    rest = kvl_match.group(2) or ""
    
    # Parse version and options
    parts = rest.split()
    if not parts:
        raise KvlParseError("Missing version in KVL header")
        
    version = parts[0]
    list_markers = ""
    options = {}
    
    # Check if second part is list markers (non-key=value, single token with special chars)
    if len(parts) > 1:
        second_part = parts[1]
        # List markers are characters like -, +, *, etc. (not key=value pairs or flags)
        if '=' not in second_part and not second_part.isalnum():
            list_markers = second_part
            option_parts = parts[2:]  # Options start from third part
        else:
            option_parts = parts[1:]  # Options start from second part
    else:
        option_parts = []
    
    # Helper to normalise option keys (kebab-case → snake_case for Python)
    def _normalise_key(raw_key: str) -> str:
        return raw_key.replace("-", "_")

    def _coerce_value(value: Any) -> Any:
        if isinstance(value, str):
            lowered = value.lower()
            if lowered in ("true", "false"):
                return lowered == "true"
        return value

    # Parse optional key=value pairs and flags
    for part in option_parts:
        if '=' in part:
            key, value = part.split('=', 1)
            normalised_key = _normalise_key(key)
            options[normalised_key] = _coerce_value(value)
        else:
            # Flag without value - assume True
            normalised_key = _normalise_key(part)
            options[normalised_key] = True

    # Handle combined spacing option if present
    if "space_around" in options:
        around_value = _coerce_value(options.pop("space_around"))
        options.setdefault("space_before", around_value)
        options.setdefault("space_after", around_value)

    # Use auto-config for spacing unless explicitly overridden
    if 'space_before' not in options and 'space_after' not in options:
        # Neither spacing option specified - use auto-config
        return auto_config_for_separator(
            separator,
            version=version,
            compact=options.get('compact', False),
            list_markers=list_markers
        )
    else:
        # At least one spacing option specified - use explicit config
        # Fill in missing spacing options with auto-config defaults
        auto_config = auto_config_for_separator(separator)
        return KvlConfig(
            separator=separator,
            version=version,
            compact=options.get('compact', False),
            space_before=options.get('space_before', auto_config.space_before),
            space_after=options.get('space_after', auto_config.space_after),
            list_markers=list_markers
        )


def generate_header(config: KvlConfig) -> str:
    """
    Generate KVL header line from configuration.
    
    Args:
        config: KvlConfig object
        
    Returns:
        Header line string
    """
    parts = [f"#{config.separator} kvl {config.version}"]
    
    if config.list_markers:
        parts.append(config.list_markers)
        
    if config.compact:
        parts.append("compact")
    
    return " ".join(parts)


def extract_content(text: str) -> str:
    """
    Extract content from KVL text, removing header if present.
    
    Args:
        text: KVL text that may contain a header
        
    Returns:
        Content without header
    """
    lines = text.splitlines()
    if not lines:
        return text
        
    first_line = lines[0].strip()
    if first_line.startswith('#') and ' kvl ' in first_line:
        # Remove header line
        return '\n'.join(lines[1:])
    
    return text
