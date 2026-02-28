"""Custom exceptions for KVL processing."""

from typing import Optional, Any


class KvlError(Exception):
    """Base exception for KVL errors."""


class KvlParseError(KvlError):
    """Error during parsing KVL text."""

    def __init__(self, message: str, line: Optional[int] = None, column: Optional[int] = None) -> None:
        self.line = line
        self.column = column
        error_location = ""
        if line is not None:
            error_location = f" at line {line}"
            if column is not None:
                error_location += f", column {column}"
        super().__init__(f"{message}{error_location}")


class KvlSerializeError(KvlError):
    """Error during serializing to KVL."""
    pass


class KvlSchemaError(KvlError):
    """Error during schema validation or processing."""
    
    def __init__(self, message: str, path: Optional[str] = None, value: Optional[Any] = None) -> None:
        self.path = path
        self.value = value
        location_info = ""
        if path is not None:
            location_info = f" at path '{path}'"
        super().__init__(f"{message}{location_info}")


class KvlValidationError(KvlSchemaError):
    """Error during data validation against schema."""
    pass


class KvlTypeError(KvlSchemaError):
    """Error during type conversion or validation."""
    pass
