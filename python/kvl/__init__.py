"""Key-Value Language (KVL) implementation in Python.

KVL is based on the original Categorical Configuration Language (CCL) by Dmitrii Kovanikov (chshersh).
See: https://c-cube.github.io/ocaml-ccl/ and https://github.com/c-cube/ocaml-ccl
"""

__version__ = "0.1.0"

from kvl.errors import (
    KvlError,
    KvlParseError,
    KvlSerializeError,
    KvlSchemaError,
    KvlValidationError,
    KvlTypeError,
)
from kvl.parser import load, loads, parse, keyvals
from kvl.serializer import dump, dumps
from kvl.schema import Schema
from kvl.config import (
    KvlConfig,
    parse_header,
    generate_header,
    extract_content,
    auto_config_for_separator,
)
from kvl.transform import compact, expand, merge

__all__ = [
    "load",
    "loads",
    "parse",
    "keyvals",
    "dump",
    "dumps",
    "KvlError",
    "KvlParseError",
    "KvlSerializeError",
    "KvlSchemaError",
    "KvlValidationError",
    "KvlTypeError",
    "Schema",
    "KvlConfig",
    "parse_header",
    "generate_header",
    "extract_content",
    "auto_config_for_separator",
    "compact",
    "expand",
    "merge",
]
