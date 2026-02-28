"""API layers and transformation tests.

Tests the different API layers: keyvals(), parse(), loads() and transformations.
"""

import pytest
import kvl


def test_keyvals_api():
    """Test keyvals() API for debugging/analysis."""
    text = """name = John
server =
    host = localhost
    port = 8080
tags = web
tags = api"""
    
    keyvals_result = kvl.keyvals(text)
    
    # Should get raw key-value pairs as single-key dicts
    assert len(keyvals_result) == 4
    assert "name" in keyvals_result[0] and keyvals_result[0]["name"] == "John"
    assert "server" in keyvals_result[1] and "host = localhost" in keyvals_result[1]["server"]
    assert "tags" in keyvals_result[2] and keyvals_result[2]["tags"] == "web"
    assert "tags" in keyvals_result[3] and keyvals_result[3]["tags"] == "api"


def test_parse_vs_loads():
    """Test difference between parse() (categorical) and loads() (compacted)."""
    text = """port = 8080
tags = web
tags = api"""
    
    # parse() returns categorical model
    parse_result = kvl.parse(text)
    assert parse_result == {
        "port": {"8080": {}},
        "tags": {
            "web": {},
            "api": {}
        }
    }
    
    # loads() returns user-friendly compacted data
    loads_result = kvl.loads(text)
    assert loads_result == {
        "port": "8080",
        "tags": ["web", "api"]
    }


def test_expand_and_merge():
    """Test expand() and merge() transformations."""
    # Test expand (user-friendly → categorical)
    user_data = {
        "port": "8080",
        "tags": ["web", "api"],
        "server": {
            "host": "localhost"
        }
    }

    expanded = kvl.expand(user_data)
    assert expanded == {
        "port": {"8080": {}},
        "tags": {"web": {}, "api": {}},
        "server": {
            "host": {"localhost": {}}
        }
    }

    # Test merge (categorical models)
    model1 = {"port": {"8080": {}}, "debug": {"true": {}}}
    model2 = {"port": {"9000": {}}, "host": {"localhost": {}}}

    merged = kvl.merge(model1, model2)
    assert merged == {
        "port": {"8080": {}, "9000": {}},
        "debug": {"true": {}},
        "host": {"localhost": {}}
    }


def test_categorical_merge_with_simple_values():
    """Test merge() with non-categorical (simple) values.

    Breaking change: Conflicting simple values now create categorical structure
    instead of last-wins behavior.
    """
    # Test merging simple string values
    model1 = {"port": "8080", "host": "localhost"}
    model2 = {"port": "8081", "timeout": "30"}

    merged = kvl.merge(model1, model2)
    # Conflicting 'port' values create categorical structure
    assert merged == {
        "port": {"8080": {}, "8081": {}},
        "host": "localhost",
        "timeout": "30"
    }

    # Compacting gives us the list representation
    compacted = kvl.compact(merged)
    assert compacted == {
        "port": ["8080", "8081"],
        "host": "localhost",
        "timeout": "30"
    }

    # Test that dict always wins over simple value
    model3 = {"port": {"nested": "value"}}
    model4 = {"port": "simple"}
    merged = kvl.merge(model3, model4)
    assert merged == {"port": {"nested": "value"}}

    merged_reverse = kvl.merge(model4, model3)
    assert merged_reverse == {"port": {"nested": "value"}}


def test_compact_transformations():
    """Test compact() transformations."""
    categorical_data = {
        "port": {"8080": {}},
        "tags": {"web": {}, "api": {}},
        "empty_key": {},
        "nested": {
            "host": {"localhost": {}}
        }
    }
    
    compacted = kvl.compact(categorical_data)
    assert compacted == {
        "port": "8080",
        "tags": ["web", "api"],
        "empty_key": {},  # Empty dict stays as empty dict
        "nested": {
            "host": "localhost"
        }
    }


def test_comment_preservation():
    """Test that /= comments are preserved as key='/' with comment values."""
    text = """/= This is a header comment
port = 8080
/= This is a middle comment
host = localhost
/= This is a footer comment"""

    result = kvl.loads(text)

    # Comments merge categorically under the "/" key
    assert "/" in result
    assert result["/"] == [
        "This is a header comment",
        "This is a middle comment",
        "This is a footer comment",
    ]

    # Regular keys should also be present
    assert result["port"] == "8080"
    assert result["host"] == "localhost"


def test_roundtrip_serialization():
    """Test full roundtrip: text → parse → compact → expand → serialize → text."""
    original_text = """server =
    host = localhost
    port = 8080
tags = web
tags = api"""
    
    # Parse to categorical model
    parsed = kvl.parse(original_text)
    
    # Compact to user-friendly
    compacted = kvl.compact(parsed)
    
    # Expand back to categorical
    expanded = kvl.expand(compacted)
    
    # Should match original parsed result
    assert expanded == parsed
    
    # Serialize back to text
    serialized_text = kvl.dumps(compacted)
    
    # Parse the serialized text
    reparsed = kvl.loads(serialized_text)
    
    # Should match compacted result
    assert reparsed == compacted


# All tests will be automatically discovered by pytest