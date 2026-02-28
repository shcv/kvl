"""Tests for the flexible recursive schema system."""

import pytest
from kvl.schema import Schema
from kvl.errors import KvlValidationError, KvlTypeError, KvlSchemaError


class TestSchemaCreation:
    """Test schema creation and basic functionality."""
    
    def test_simple_field_specs(self):
        """Test creation with simple field specifications."""
        schema = Schema({
            'population': int,
            'capital': str,
            'active': bool
        })
        
        assert 'population' in schema.fields
        assert 'capital' in schema.fields
        assert 'active' in schema.fields
        assert schema.open is True
        assert schema.required is False
    
    def test_schema_modes(self):
        """Test open/closed and required/optional modes."""
        # Open, optional (default)
        schema1 = Schema({'test': str})
        assert schema1.open is True
        assert schema1.required is False
        
        # Closed, required
        schema2 = Schema({'test': str}, open=False, required=True)
        assert schema2.open is False
        assert schema2.required is True
    
    def test_constraint_specs(self):
        """Test creation with constraint specifications."""
        schema = Schema({
            'port': {
                'type': 'int',
                'min': 1,
                'max': 65535
            },
            'name': {
                'type': 'string',
                'max-length': 50,
                'pattern': r'^[A-Za-z]+$'
            }
        })
        
        assert 'port' in schema.fields
        assert 'name' in schema.fields
    
    def test_lambda_constraints(self):
        """Test creation with lambda constraint functions."""
        schema = Schema({
            'port': (int, lambda v: 1 <= v <= 65535),
            'name': (str, lambda v: len(v) > 0)
        })
        
        assert 'port' in schema.fields
        assert 'name' in schema.fields
    
    def test_nested_schemas(self):
        """Test creation with nested schema specifications."""
        schema = Schema({
            'global_field': str,
            'server': {
                'host': str,
                'port': {
                    'type': 'int',
                    'min': 8000,
                    'max': 9000
                }
            }
        })
        
        assert 'global_field' in schema.fields
        assert 'server' in schema.fields
        assert isinstance(schema.fields['server'], dict)


class TestFieldOrientedValidation:
    """Test field-oriented validation with dynamic keys."""
    
    def test_states_example(self):
        """Test validation of states data with dynamic state names."""
        schema = Schema({
            'population': int,
            'capital': str,
            'median_income': int
        })
        
        data = {
            'Alabama': {
                'population': '5224279',
                'capital': 'Montgomery',
                'median_income': '52035'
            },
            'Alaska': {
                'population': '740339',
                'capital': 'Juneau',
                'median_income': '75463'
            }
        }
        
        result = schema.deserialize(data)
        
        # Check type conversions
        assert result['Alabama']['population'] == 5224279
        assert isinstance(result['Alabama']['population'], int)
        assert result['Alabama']['capital'] == 'Montgomery'
        assert isinstance(result['Alabama']['capital'], str)
        
        assert result['Alaska']['population'] == 740339
        assert isinstance(result['Alaska']['population'], int)
    
    def test_global_field_matching(self):
        """Test that global fields match anywhere in the structure."""
        schema = Schema({
            'timeout': int,
            'enabled': bool
        })
        
        data = {
            'service1': {
                'timeout': '30',
                'enabled': 'true'
            },
            'service2': {
                'monitoring': {
                    'timeout': '60',
                    'enabled': 'false'
                }
            }
        }
        
        result = schema.deserialize(data)
        
        # Check deep nesting works
        assert result['service1']['timeout'] == 30
        assert result['service1']['enabled'] is True
        assert result['service2']['monitoring']['timeout'] == 60
        assert result['service2']['monitoring']['enabled'] is False
    
    def test_open_schema_unknown_fields(self):
        """Test that open schemas allow unknown fields to pass through."""
        schema = Schema({'known_field': int}, open=True)
        
        data = {
            'state1': {
                'known_field': '123',
                'unknown_field': 'test',
                'another_unknown': {'nested': 'data'}
            }
        }
        
        result = schema.deserialize(data)
        
        assert result['state1']['known_field'] == 123
        assert result['state1']['unknown_field'] == 'test'
        assert result['state1']['another_unknown'] == {'nested': 'data'}
    
    def test_closed_schema_rejects_unknown_fields(self):
        """Test that closed schemas reject unknown fields."""
        schema = Schema({'known_field': int}, open=False)
        
        data = {'unknown_field': 'test'}
        
        with pytest.raises(KvlValidationError, match="Unknown field 'unknown_field' in closed schema"):
            schema.deserialize(data)


class TestHierarchicalScoping:
    """Test hierarchical field scoping and context-specific overrides."""
    
    def test_context_specific_overrides(self):
        """Test that context-specific specs override global ones."""
        schema = Schema({
            'port': int,  # Global default
            'server': {
                'port': {
                    'type': 'int',
                    'min': 8000,
                    'max': 9000
                }
            },
            'database': {
                'port': {
                    'type': 'int',
                    'enum': '3306,5432'
                }
            }
        })
        
        # Valid server port (in range 8000-9000)
        data1 = {'server': {'port': '8080'}}
        result1 = schema.deserialize(data1)
        assert result1['server']['port'] == 8080
        
        # Valid database port (in enum)
        data2 = {'database': {'port': '3306'}}
        result2 = schema.deserialize(data2)
        assert result2['database']['port'] == 3306
        
        # Invalid server port (out of range)
        with pytest.raises(KvlValidationError):
            schema.deserialize({'server': {'port': '80'}})
        
        # Invalid database port (not in enum)
        with pytest.raises(KvlValidationError):
            schema.deserialize({'database': {'port': '8080'}})
    
    def test_fallback_to_global_specs(self):
        """Test fallback to global specs when no context-specific spec exists."""
        schema = Schema({
            'timeout': int,
            'enabled': bool,
            'server': {
                'host': str
                # No timeout or enabled specs here
            }
        })
        
        data = {
            'server': {
                'host': 'localhost',
                'timeout': '30',    # Should use global timeout spec
                'enabled': 'true'   # Should use global enabled spec
            }
        }
        
        result = schema.deserialize(data)
        assert result['server']['host'] == 'localhost'
        assert result['server']['timeout'] == 30
        assert result['server']['enabled'] is True


class TestConstraintValidation:
    """Test various constraint types and validation rules."""
    
    def test_numeric_constraints(self):
        """Test min/max constraints for numeric values."""
        schema = Schema({
            'percentage': {
                'type': 'float',
                'min': 0.0,
                'max': 100.0
            }
        })
        
        # Valid values
        assert schema.deserialize({'test': {'percentage': '50.5'}})['test']['percentage'] == 50.5
        assert schema.deserialize({'test': {'percentage': '0.0'}})['test']['percentage'] == 0.0
        assert schema.deserialize({'test': {'percentage': '100.0'}})['test']['percentage'] == 100.0
        
        # Invalid values
        with pytest.raises(KvlValidationError):
            schema.deserialize({'test': {'percentage': '-1.0'}})
        
        with pytest.raises(KvlValidationError):
            schema.deserialize({'test': {'percentage': '101.0'}})
    
    def test_string_constraints(self):
        """Test string length and pattern constraints."""
        schema = Schema({
            'name': {
                'type': 'string',
                'min-length': 2,
                'max-length': 10,
                'pattern': r'^[A-Za-z]+$'
            }
        })
        
        # Valid values
        assert schema.deserialize({'test': {'name': 'Alice'}})['test']['name'] == 'Alice'
        
        # Too short
        with pytest.raises(KvlValidationError):
            schema.deserialize({'test': {'name': 'A'}})
        
        # Too long
        with pytest.raises(KvlValidationError):
            schema.deserialize({'test': {'name': 'VeryLongName'}})
        
        # Invalid pattern
        with pytest.raises(KvlValidationError):
            schema.deserialize({'test': {'name': 'Alice123'}})
    
    def test_enum_constraints(self):
        """Test enumeration constraints."""
        schema = Schema({
            'environment': {
                'type': 'string',
                'enum': 'development,staging,production'
            }
        })
        
        # Valid values
        assert schema.deserialize({'test': {'environment': 'development'}})['test']['environment'] == 'development'
        assert schema.deserialize({'test': {'environment': 'staging'}})['test']['environment'] == 'staging'
        
        # Invalid value
        with pytest.raises(KvlValidationError):
            schema.deserialize({'test': {'environment': 'testing'}})
    
    def test_list_constraints(self):
        """Test list/array size constraints."""
        schema = Schema({
            'tags': {
                'type': 'list',
                'min-items': 1,
                'max-items': 3
            }
        })
        
        # Valid lists (using KVL categorical format)
        data1 = {'test': {'tags': {'web': {}, 'api': {}}}}
        result1 = schema.deserialize(data1)
        assert result1['test']['tags'] == ['web', 'api']
        
        # Too few items
        with pytest.raises(KvlValidationError):
            schema.deserialize({'test': {'tags': {}}})
        
        # Too many items  
        with pytest.raises(KvlValidationError):
            schema.deserialize({'test': {'tags': {'a': {}, 'b': {}, 'c': {}, 'd': {}}}})


class TestRequiredFields:
    """Test required field validation."""
    
    def test_schema_level_required(self):
        """Test schema-level required setting."""
        schema = Schema({
            'server': {
                'host': str,
                'port': int
            }
        }, required=True)
        
        # Valid - all fields present
        data1 = {'server': {'host': 'localhost', 'port': '8080'}}
        result = schema.deserialize(data1)
        assert result['server']['host'] == 'localhost'
        assert result['server']['port'] == 8080
        
        # Invalid - missing required field
        with pytest.raises(KvlValidationError, match="Required field 'port' is missing"):
            schema.deserialize({'server': {'host': 'localhost'}})
    
    def test_field_level_required_override(self):
        """Test per-field required overrides."""
        schema = Schema({
            'server': {
                'host': {
                    'type': 'string',
                    'required': 'true'
                },
                'port': {
                    'type': 'int', 
                    'required': 'false'
                }
            }
        }, required=False)  # Schema default is optional
        
        # Valid - only required field present
        result = schema.deserialize({'server': {'host': 'localhost'}})
        assert result['server']['host'] == 'localhost'
        
        # Invalid - missing required field
        with pytest.raises(KvlValidationError, match="Required field 'host' is missing"):
            schema.deserialize({'server': {'port': '8080'}})


class TestSchemaFiles:
    """Test loading schemas from KVL files."""
    
    def test_states_schema_file(self):
        """Test loading and using the states.schema.kvl file."""
        schema = Schema.from_file('examples/states.schema.kvl')
        
        # Test with sample states data
        data = {
            'TestState': {
                'population': '1000000',
                'capital': 'TestCity',
                'white': '65.5',
                'black': '20.0',
                'founded': '1800'  # Unknown field, should pass through
            }
        }
        
        result = schema.deserialize(data)
        
        assert result['TestState']['population'] == 1000000
        assert isinstance(result['TestState']['population'], int)
        assert result['TestState']['white'] == 65.5
        assert isinstance(result['TestState']['white'], float)
        assert result['TestState']['founded'] == '1800'  # Passed through
    
    def test_server_schema_file(self):
        """Test loading and using the server.schema.kvl file."""
        schema = Schema.from_file('examples/server.schema.kvl')
        
        # Valid configuration
        data = {
            'server': {
                'name': 'api-server',
                'port': '8080'
            },
            'database': {
                'host': 'localhost',
                'port': '3306',
                'name': 'myapp_db'
            },
            'monitoring': {
                'enabled': 'true'
            }
        }
        
        result = schema.deserialize(data)
        
        assert result['server']['name'] == 'api-server'
        assert result['server']['port'] == 8080
        assert result['database']['port'] == 3306
        assert result['monitoring']['enabled'] is True
    
    def test_schema_header_parsing(self):
        """Test parsing of schema headers for open/closed and required/optional."""
        # Test closed required schema
        schema_text1 = """
#= kvl 1.0 schema closed required

test_field = string
"""
        schema1 = Schema.from_kvl(schema_text1)
        assert schema1.open is False
        assert schema1.required is True
        
        # Test open optional schema (default)
        schema_text2 = """
#= kvl 1.0 schema

test_field = string
"""
        schema2 = Schema.from_kvl(schema_text2)
        assert schema2.open is True
        assert schema2.required is False


class TestTypeConversion:
    """Test automatic type conversion."""
    
    def test_basic_type_conversions(self):
        """Test conversion of basic types."""
        schema = Schema({
            'int_field': int,
            'float_field': float,
            'bool_field': bool,
            'str_field': str
        })
        
        data = {
            'test': {
                'int_field': '123',
                'float_field': '45.67',
                'bool_field': 'true',
                'str_field': 'hello'
            }
        }
        
        result = schema.deserialize(data)
        
        assert result['test']['int_field'] == 123
        assert isinstance(result['test']['int_field'], int)
        
        assert result['test']['float_field'] == 45.67
        assert isinstance(result['test']['float_field'], float)
        
        assert result['test']['bool_field'] is True
        assert isinstance(result['test']['bool_field'], bool)
        
        assert result['test']['str_field'] == 'hello'
        assert isinstance(result['test']['str_field'], str)
    
    def test_bool_conversion_variants(self):
        """Test various boolean string formats."""
        schema = Schema({'enabled': bool})
        
        # True variants
        for true_val in ['true', 'True', 'yes', 'YES', '1', 'on']:
            result = schema.deserialize({'test': {'enabled': true_val}})
            assert result['test']['enabled'] is True
        
        # False variants
        for false_val in ['false', 'False', 'no', 'NO', '0', 'off']:
            result = schema.deserialize({'test': {'enabled': false_val}})
            assert result['test']['enabled'] is False
    
    def test_list_conversion(self):
        """Test conversion of KVL categorical format to lists."""
        schema = Schema({'tags': list})
        
        # KVL categorical format
        data = {'test': {'tags': {'web': {}, 'api': {}, 'prod': {}}}}
        result = schema.deserialize(data)
        
        assert result['test']['tags'] == ['web', 'api', 'prod']
        assert isinstance(result['test']['tags'], list)
    
    def test_type_conversion_errors(self):
        """Test handling of type conversion errors."""
        schema = Schema({'number': int})
        
        with pytest.raises(KvlTypeError, match="Cannot convert 'not_a_number' to int"):
            schema.deserialize({'test': {'number': 'not_a_number'}})


class TestErrorHandling:
    """Test error handling and reporting."""
    
    def test_validation_error_paths(self):
        """Test that validation errors include correct field paths."""
        schema = Schema({
            'server': {
                'port': {
                    'type': 'int',
                    'min': 1000
                }
            }
        })
        
        try:
            schema.deserialize({'server': {'port': '80'}})
            assert False, "Expected validation error"
        except KvlValidationError as e:
            assert 'server.port' in str(e)
    
    def test_schema_definition_errors(self):
        """Test errors in schema definitions."""
        # Unknown type
        with pytest.raises(KvlSchemaError, match="Unknown type"):
            Schema.from_kvl('field = unknown_type')
        
        # Missing type in constraint spec
        with pytest.raises(KvlSchemaError, match="missing 'type' field"):
            Schema({'field': {'min': 1, 'max': 10}})


class TestIntegrationWithRealData:
    """Integration tests with real data files."""
    
    def test_full_states_file_validation(self):
        """Test validation of the complete states.kvl file."""
        schema = Schema.from_file('examples/states.schema.kvl')
        result = schema.load('examples/states.kvl')
        
        # Should have all 50 states
        assert len(result) == 50
        
        # Check a few specific states
        assert 'Alabama' in result
        assert 'California' in result
        assert 'Texas' in result
        
        # Check type conversions
        assert isinstance(result['Alabama']['population'], int)
        assert isinstance(result['California']['white'], float)
        assert isinstance(result['Texas']['capital'], str)
    
    def test_schema_serialization_roundtrip(self):
        """Test that serialization and deserialization are consistent."""
        schema = Schema({
            'population': int,
            'percentage': float,
            'active': bool,
            'tags': list
        })
        
        original_data = {
            'state1': {
                'population': 1000000,
                'percentage': 65.5,
                'active': True,
                'tags': ['tag1', 'tag2']
            }
        }
        
        # Serialize to KVL format
        kvl_data = schema.serialize(original_data)
        
        # Deserialize back
        result = schema.deserialize(kvl_data)
        
        # Should be equivalent to original
        assert result['state1']['population'] == 1000000
        assert result['state1']['percentage'] == 65.5
        assert result['state1']['active'] is True
        assert result['state1']['tags'] == ['tag1', 'tag2']


if __name__ == '__main__':
    pytest.main([__file__])