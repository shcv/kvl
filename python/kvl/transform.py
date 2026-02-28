"""Structural transformation utilities for KVL.

Provides compacting, expansion, and merging transformations for KVL data structures.

Based on the original Categorical Configuration Language (CCL) by Dmitrii Kovanikov (chshersh).
See: https://c-cube.github.io/ocaml-ccl/ and https://github.com/c-cube/ocaml-ccl
"""

from typing import Dict, List, Any, Union
from kvl.errors import KvlParseError
from kvl.config import KvlConfig
from kvl.utils import ensure_config


def compact(data: Any, config: KvlConfig = None) -> Any:
    """
    Compact KVL structures by applying two transformations:

    1. Singleton empty key lifting: {'': content} → content (removes empty layer)
    2. Empty-value dict flattening: {'key1': '', 'key2': ''} → ['key1', 'key2']

    Args:
        data: Parsed KVL data (nested dictionaries)
        config: KVL configuration (used for separator detection in section headers)

    Returns:
        Compacted data structure

    Raises:
        KvlParseError: If mixed structures are detected that shouldn't be compacted
    """
    config = ensure_config(config, KvlConfig)

    if isinstance(data, list):
        # Recursively compact list items
        return [compact(item, config) for item in data]
    elif not isinstance(data, dict):
        return data

    # First, recursively compact nested structures
    compacted = {}
    for key, value in data.items():
        compacted[key] = compact(value, config)

    # Then apply compacting rules to this level
    return _apply_compacting_rules(compacted, config)


def _apply_compacting_rules(data: Dict[str, Any], config: KvlConfig) -> Any:
    """
    Apply compacting rules to a dictionary.

    Rule 1: Singleton empty key lifting: {'': content} → content
    Rule 2: Single empty child conversion: {'value': {}} → 'value' (string conversion)
    Rule 3: Empty-value dict flattening: {'key1': {}, 'key2': {}} → ['key1', 'key2']
    Rule 4: Section headers are not flattened

    Returns:
        Compacted structure or original dict if no rules apply
    """
    if not isinstance(data, dict) or not data:
        return data

    # Rule 1: Singleton empty key lifting
    if len(data) == 1 and "" in data:
        empty_value = data[""]
        if (
            isinstance(empty_value, dict)
            and empty_value
            and all(isinstance(v, dict) for v in empty_value.values())
        ):
            return [{key: value} for key, value in empty_value.items()]
        return empty_value  # Lift the content, removing the empty key layer

    # Rule 2: Single empty child conversion (string conversion from categorical model)
    if len(data) == 1:
        child_key = next(iter(data.keys()))
        child_value = next(iter(data.values()))
        if isinstance(child_value, dict) and not child_value and child_key != "":
            # Single empty child = convert to string value (unless key is empty)
            return child_key

    # Rule 3: Empty-value dict flattening
    # Check if all values are empty dicts (indicating a flattening candidate)
    if all(isinstance(v, dict) and not v for v in data.values()):
        # Rule 4: Don't flatten if any key looks like a section header
        for key in data.keys():
            if isinstance(key, str) and _is_section_header(key, config.separator):
                return data  # Keep as dict if we have section headers

        # Flatten to list of keys
        return list(data.keys())

    # No compacting rules apply
    return data


def _is_section_header(value: str, separator: str) -> bool:
    """
    Check if a value appears to be a section header (starts with separator).

    Examples:
        "== Section ==" with separator "="
        ":: section ::" with separator ":"
        "-> heading <-" with separator "->"
    """
    if not isinstance(value, str) or not value.strip():
        return False

    stripped = value.strip()
    return stripped.startswith(separator)


def expand(data: Any) -> Any:
    """
    Expand lists back to KVL nested dictionary format for serialization.

    This is the inverse of compact() - converts Python lists back to
    the categorical nested dictionary structure that KVL expects.

    Args:
        data: Data that may contain lists

    Returns:
        Data with lists converted to nested dict format
    """
    if isinstance(data, list):
        # Convert list to nested dict with empty dict values (CCL-consistent)
        result = {}
        for item in data:
            if isinstance(item, str):
                result[item] = {}
            else:
                expanded_item = expand(item)
                if "" not in result:
                    result[""] = {}
                result[""] = merge(result[""], expanded_item)
        return result

    elif isinstance(data, dict):
        # Recursively process nested dictionaries
        result = {}
        for key, value in data.items():
            result[key] = expand(value)
        return result

    else:
        # Primitive values (strings, numbers, etc.) convert to categorical format
        if data is None:
            return {}
        else:
            return {str(data): {}}


def merge(model1: Any, model2: Any) -> Any:
    """Merge two categorical models recursively.

    When both arguments are dicts, performs recursive merge.
    When values conflict (both are non-dict), converts them to categorical format
    and merges the resulting structures.

    This implements the categorical merge semantics where conflicting simple values
    create categorical structure: port=8080 + port=8081 → {port: {8080: {}, 8081: {}}}

    Args:
        model1: First model (dict or any value)
        model2: Second model (dict or any value)

    Returns:
        Merged categorical structure
    """
    # If both are dicts, merge recursively
    if isinstance(model1, dict) and isinstance(model2, dict):
        result = model1.copy()
        for k, v in model2.items():
            result[k] = merge(result[k], v) if k in result else v
        return result

    # If only one is a dict, the dict wins (it's more structured)
    if isinstance(model1, dict):
        return model1
    if isinstance(model2, dict):
        return model2

    # Both are non-dict values - convert to categorical and merge
    # This handles conflicting simple values by creating categorical structure
    cat1 = {str(model1): {}} if model1 not in (None, '') else {}
    cat2 = {str(model2): {}} if model2 not in (None, '') else {}
    return merge(cat1, cat2)
