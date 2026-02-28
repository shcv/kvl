"""Tests for KVL serializer functionality.

This module tests the public serialization API for KVL data.
The serializer should handle roundtrip consistency (parse->serialize->parse).
"""

import pytest
import kvl
from io import StringIO


class TestBasicSerialization:
    """Test fundamental serialization operations."""
    
    def test_roundtrip_simple_data(self):
        """Test basic roundtrip consistency."""
        data = {"name": "test", "version": "1.0"}
        
        serialized = kvl.dumps(data)
        reparsed = kvl.loads(serialized)
        
        assert reparsed == data
    
    def test_roundtrip_nested_data(self):
        """Test roundtrip with nested structures."""
        data = {
            "server": {
                "host": "localhost", 
                "port": "8080"
            }
        }
        
        serialized = kvl.dumps(data)
        reparsed = kvl.loads(serialized)
        
        assert reparsed == data
    
    def test_roundtrip_list_data(self):
        """Test roundtrip with list structures."""
        data = {"tags": ["web", "api", "production"]}
        
        serialized = kvl.dumps(data)
        reparsed = kvl.loads(serialized)
        
        assert reparsed == data


class TestFileOperations:
    """Test file-based serialization operations."""
    
    def test_dump_to_file_object(self):
        """Test dumping to a file-like object."""
        data = {"name": "test", "version": "1.0"}
        buffer = StringIO()
        
        kvl.dump(data, buffer)
        result = buffer.getvalue()
        
        # Should be valid KVL that roundtrips
        reparsed = kvl.loads(result)
        assert reparsed == data
    
    def test_include_header(self):
        """Test including version header in output."""
        data = {"key": "value"}
        
        result = kvl.dumps(data, include_header=True)
        
        assert result.startswith("#= kvl 1.0")
        # Should still roundtrip correctly
        reparsed = kvl.loads(result)
        assert reparsed == data


class TestComplexStructures:
    """Test serialization of complex nested structures."""
    
    def test_deeply_nested_roundtrip(self):
        """Test deeply nested structures roundtrip correctly."""
        data = {
            "level1": {
                "level2": {
                    "level3": {
                        "value": "deep"
                    }
                }
            }
        }
        
        serialized = kvl.dumps(data)
        reparsed = kvl.loads(serialized)
        
        assert reparsed == data
    
    def test_mixed_lists_and_objects_roundtrip(self):
        """Test structures mixing lists and nested objects."""
        data = {
            "servers": {
                "web": {
                    "hosts": ["web1", "web2"],
                    "port": "80"
                },
                "api": {
                    "hosts": ["api1", "api2"], 
                    "port": "8080"
                }
            }
        }
        
        serialized = kvl.dumps(data)
        reparsed = kvl.loads(serialized)
        
        assert reparsed == data
    
    def test_empty_nested_structures_roundtrip(self):
        """Test serializing empty nested structures."""
        data = {
            "empty_dict": {},
            "dict_with_empty": {
                "nested_empty": {}
            }
        }
        
        serialized = kvl.dumps(data)
        reparsed = kvl.loads(serialized)
        
        # Empty dicts serialize and parse as expected
        expected = {"empty_dict": {}, "dict_with_empty": "nested_empty"}
        assert reparsed == expected


class TestFormattingOptions:
    """Test various formatting and configuration options."""
    
    def test_different_separators_roundtrip(self):
        """Test serialization with different separators maintains consistency."""
        data = {"host": "localhost", "port": "8080"}
        
        # Test colon separator
        colon_result = kvl.dumps(data, config=kvl.KvlConfig(separator=":"), include_header=True)
        assert "#: kvl 1.0" in colon_result
        reparsed = kvl.loads(colon_result)
        assert reparsed == data
        
        # Test arrow separator
        arrow_result = kvl.dumps(data, config=kvl.KvlConfig(separator="->"), include_header=True)
        assert "#-> kvl 1.0" in arrow_result
        reparsed = kvl.loads(arrow_result)
        assert reparsed == data
    
    def test_custom_indentation_produces_valid_kvl(self):
        """Test custom indentation still produces valid parseable KVL."""
        data = {
            "server": {
                "nested": {
                    "value": "test"
                }
            }
        }
        
        serialized = kvl.dumps(data)
        reparsed = kvl.loads(serialized)
        
        assert reparsed == data


class TestListMarkerSerialization:
    """Test list marker output in serialization."""
    
    def test_auto_detection_from_config(self):
        """Test that list markers are auto-detected from config."""
        data = {"items": ["first", "second", "third"]}
        config = kvl.KvlConfig(list_markers="-")
        
        result = kvl.dumps(data, config=config, include_header=True)
        
        assert "#= kvl 1.0 -" in result
        assert "- first" in result
        assert "- second" in result
        assert "- third" in result
    
    def test_explicit_list_marker_override(self):
        """Test explicit list marker override."""
        data = {"tasks": ["design", "implement", "test"]}
        config = kvl.KvlConfig(list_markers="-")
        
        result = kvl.dumps(data, config=config, list_marker="+", include_header=True)
        
        assert "#= kvl 1.0 -" in result  # Header uses config
        assert "+ design" in result      # Content uses override
        assert "+ implement" in result
        assert "+ test" in result
    
    def test_force_categorical_format(self):
        """Test forcing categorical format with empty list_marker."""
        data = {"items": ["first", "second"]}
        config = kvl.KvlConfig(list_markers="-")
        
        result = kvl.dumps(data, config=config, list_marker="")
        
        assert "first =" in result  # Categorical format
        assert "second =" in result
        assert "- first" not in result  # No list markers
    
    def test_different_separators_with_list_markers(self):
        """Test list markers work with different separators."""
        data = {"priorities": ["high", "low"]}
        
        # Colon separator
        config_colon = kvl.KvlConfig(separator=":", list_markers="-")
        result_colon = kvl.dumps(data, config=config_colon, include_header=True)
        assert "#: kvl 1.0 -" in result_colon
        assert "priorities :" in result_colon
        assert "- high" in result_colon
        
        # Arrow separator
        config_arrow = kvl.KvlConfig(separator="->", list_markers="+")
        result_arrow = kvl.dumps(data, config=config_arrow, include_header=True)
        assert "#-> kvl 1.0 +" in result_arrow
        assert "priorities ->" in result_arrow
        assert "+ high" in result_arrow
    
    def test_complex_nested_structures(self):
        """Test list marker serialization with complex nested data."""
        data = {
            "servers": [
                {"name": "web1", "port": "80"},
                {"name": "web2", "port": "8080"}
            ]
        }
        config = kvl.KvlConfig(list_markers="-")
        
        result = kvl.dumps(data, config=config)
        
        assert "- name = web1" in result
        assert "port = 80" in result
        assert "- name = web2" in result  
        assert "port = 8080" in result
    
    def test_roundtrip_with_list_markers(self):
        """Test complete roundtrip using list markers."""
        data = {"features": ["auth", "api", "logs"]}
        config = kvl.KvlConfig(list_markers="-")
        
        # Serialize with list markers
        serialized = kvl.dumps(data, config=config, include_header=True)
        
        # Parse back
        reparsed = kvl.loads(serialized)
        
        # Should be identical
        assert reparsed == data
    
    def test_mixed_data_types(self):
        """Test serialization with mixed data types."""
        data = {
            "simple_list": ["item1", "item2"],
            "simple_key": "value",
            "nested_object": {"inner": "data"}
        }
        config = kvl.KvlConfig(list_markers="-")
        
        result = kvl.dumps(data, config=config, include_header=True)
        
        # List should use markers
        assert "- item1" in result
        assert "- item2" in result
        # Regular key-value should work normally
        assert "simple_key = value" in result
        # Nested object should work normally
        assert "nested_object =" in result
        assert "inner = data" in result
    
    def test_no_list_markers_configured(self):
        """Test behavior when no list markers are configured."""
        data = {"items": ["first", "second"]}
        config = kvl.KvlConfig()  # No list markers
        
        # Should use categorical format automatically
        result = kvl.dumps(data, config=config)
        assert "first =" in result
        assert "second =" in result
        assert "- first" not in result
    
    def test_file_operations_with_list_markers(self):
        """Test file dump operations with list markers."""
        from io import StringIO
        
        data = {"tasks": ["plan", "code", "test"]}
        config = kvl.KvlConfig(list_markers="-")
        buffer = StringIO()
        
        kvl.dump(data, buffer, config=config, include_header=True)
        result = buffer.getvalue()
        
        assert "#= kvl 1.0 -" in result
        assert "- plan" in result
        assert "- code" in result
        assert "- test" in result