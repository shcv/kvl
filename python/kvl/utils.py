"""Utility functions for KVL processing.

Based on the original Categorical Configuration Language (CCL) by Dmitrii Kovanikov (chshersh).
See: https://c-cube.github.io/ocaml-ccl/ and https://github.com/c-cube/ocaml-ccl
"""

from typing import Any, Dict, List, Union, TextIO, Callable, TypeVar, Optional
from pathlib import Path

# Type definitions
KvlValue = Union[str, Dict[str, "KvlValue"]]
FileOrPath = Union[str, Path, TextIO]
T = TypeVar('T')


def handle_read(file_or_path: FileOrPath) -> str:
    """
    Read content from a file path or file-like object.
    
    Args:
        file_or_path: File path string, Path object, or file-like object
        
    Returns:
        String content of the file
        
    Raises:
        IOError: If the file cannot be read
    """
    if isinstance(file_or_path, (str, Path)):
        with open(file_or_path, "r") as f:
            return f.read()
    else:
        return file_or_path.read()


def handle_write(file_or_path: FileOrPath, content: str) -> None:
    """
    Write content to a file path or file-like object.
    
    Args:
        file_or_path: File path string, Path object, or file-like object
        content: String content to write
        
    Raises:
        IOError: If the file cannot be written
    """
    if isinstance(file_or_path, (str, Path)):
        with open(file_or_path, "w") as f:
            f.write(content)
    else:
        file_or_path.write(content)


def ensure_config(config: Optional[T], default_factory: Callable[[], T]) -> T:
    """Ensure config is not None, creating default if needed."""
    return config if config is not None else default_factory()
