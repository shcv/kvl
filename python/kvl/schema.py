"""Flexible recursive schema system for KVL with field-oriented validation.

KVL (Key-Value Language) is derived from CCL (Categorical Configuration Language)
by Dmitrii Kovanikov (chshersh). This implementation provides flexible, recursive schema validation 
using field-oriented matching that works with any data structure.
"""

from typing import Any, Dict, List, Callable, Union, Tuple, Optional
from datetime import datetime
from decimal import Decimal
import re

from kvl.errors import KvlSchemaError, KvlValidationError, KvlTypeError
from kvl.config import KvlConfig
from kvl.utils import handle_read, FileOrPath
from kvl.parser import loads, load
from kvl.serializer import dumps, dump


class Schema:
    """Flexible recursive schema for validating KVL data with field-oriented matching."""
    
    def __init__(self, fields: Dict[str, Any], open: bool = True, required: bool = False):
        """
        Create a flexible schema with field specifications.
        
        Args:
            fields: Dict where keys are field names and values can be:
                - A type (e.g., int, str, bool)
                - A constraint spec dict (e.g., {"type": int, "min": 1, "max": 100})
                - A tuple of (type, constraint_function)
                - A nested schema dict for subschema definitions
            open: If True, allow unmatched fields to pass through (default: True)
            required: If True, fields in matched subschemas must be present (default: False)
        
        Examples:
            # Global field specs (apply anywhere)
            Schema({
                "population": int,
                "capital": str,
                "active": bool
            })
            
            # Mixed global and context-specific specs
            Schema({
                "port": int,  # Global default
                "server": {
                    "port": {"type": int, "min": 8000, "max": 9000}  # Context-specific
                }
            })
            
            # Strict validation
            Schema(fields, open=False, required=True)
        """
        self.fields = self._normalize_fields(fields)
        self.open = open
        self.required = required
    
    def _normalize_fields(self, fields: Dict[str, Any]) -> Dict[str, Any]:
        """Normalize field specifications into consistent format."""
        normalized = {}
        
        for field_name, field_spec in fields.items():
            if isinstance(field_spec, type):
                # Simple type: "port = int"
                normalized[field_name] = field_spec
            elif isinstance(field_spec, tuple) and len(field_spec) >= 2:
                # Lambda constraint: "port = (int, lambda v: v > 0)" or parsed constraint spec
                normalized[field_name] = field_spec
            elif isinstance(field_spec, dict):
                if self._is_constraint_spec(field_spec):
                    # Complex constraint: "port = {type: int, min: 1, max: 100}"
                    normalized[field_name] = self._parse_constraint_spec(field_spec)
                else:
                    # Nested schema: "server = {port: int, host: str}"
                    normalized[field_name] = self._normalize_fields(field_spec)
            else:
                raise KvlSchemaError(f"Invalid field specification for '{field_name}': {field_spec}")
        
        return normalized
    
    def _is_constraint_spec(self, spec_dict: Dict[str, Any]) -> bool:
        """Check if dict contains constraint keywords vs nested field specs."""
        constraint_keywords = {
            'type', 'min', 'max', 'pattern', 'enum', 'required',
            'min-length', 'max-length', 'min_length', 'max_length',
            'min-items', 'max-items', 'min_items', 'max_items'
        }
        return any(key in constraint_keywords for key in spec_dict.keys())
    
    def _parse_constraint_spec(self, spec_dict: Dict[str, Any]) -> Union[type, Tuple[type, Callable, Dict[str, Any]]]:
        """Parse a constraint specification into (type, constraint_function, constraints_dict)."""
        if 'type' not in spec_dict:
            raise KvlSchemaError(f"Constraint spec missing 'type' field: {spec_dict}")
        
        base_type = self._parse_type_name(spec_dict['type'])
        constraints = {k: v for k, v in spec_dict.items() if k != 'type'}
        
        if constraints:
            constraint_fn = self._build_constraint_function(constraints)
            return (base_type, constraint_fn, constraints)
        else:
            return base_type
    
    def validate(self, data: Dict[str, Any], path: str = "") -> bool:
        """
        Recursively validate data against schema without conversion.
        
        Args:
            data: Dictionary to validate (with any nesting structure)
            path: Current path for error reporting
            
        Returns:
            True if validation passes
            
        Raises:
            KvlValidationError: If validation fails
            KvlTypeError: If type conversion fails
        """
        try:
            self.deserialize(data, path)
            return True
        except (KvlValidationError, KvlTypeError):
            raise
    
    def deserialize(self, data: Dict[str, Any], path: str = "") -> Dict[str, Any]:
        """
        Recursively validate and convert data to typed Python dict.
        
        Algorithm:
        1. For each key in data, check against ALL root schema fields
        2. If match found, apply that validation recursively  
        3. If no match found, handle based on open/closed setting
        4. Within matched subschemas, check subschema fields first, then root fields
        5. Validate required fields in matched subschemas
        
        Args:
            data: Raw data dictionary (with any nesting structure)
            path: Current path for error reporting
            
        Returns:
            Typed Python dictionary with recursive validation applied
            
        Raises:
            KvlValidationError: If validation fails
            KvlTypeError: If type conversion fails
        """
        if not isinstance(data, dict):
            raise KvlTypeError(f"Expected dict, got {type(data).__name__}", path, data)
        
        return self._validate_recursive(data, None, path.split('.') if path else [])
    
    def _validate_recursive(self, data: Dict[str, Any], current_subschema: Optional[Dict[str, Any]], path: List[str]) -> Dict[str, Any]:
        """Core recursive validation algorithm."""
        result = {}
        
        for key, value in data.items():
            current_path = path + [key]
            field_path = '.'.join(current_path)
            
            # Check for matches in current subschema first, then root fields
            field_spec = None
            
            if current_subschema and key in current_subschema:
                # Found in current subschema
                field_spec = current_subschema[key]
            elif key in self.fields:
                # Found in root schema
                field_spec = self.fields[key]
            
            if field_spec is not None:
                # Found a matching field spec
                result[key] = self._validate_value_against_spec(value, field_spec, current_path)
            else:
                # No matching field spec
                if not self.open:
                    raise KvlValidationError(f"Unknown field '{key}' in closed schema", field_path, value)
                elif isinstance(value, dict):
                    # Descend into nested structure without specific schema
                    result[key] = self._validate_recursive(value, None, current_path)
                else:
                    # Pass through leaf value
                    result[key] = value
        
        # Check required fields if we're in a subschema context
        if current_subschema is not None:
            self._check_required_fields(data, current_subschema, path)
        
        return result
    
    def _is_field_spec(self, spec: Any) -> bool:
        """Check if spec is a field specification vs nested subschema."""
        if isinstance(spec, (type, tuple)):
            return True
        elif isinstance(spec, dict):
            return self._is_constraint_spec(spec)
        return False
    
    def _validate_value_against_spec(self, value: Any, spec: Any, path: List[str]) -> Any:
        """Validate a single value against its specification."""
        field_path = '.'.join(path)
        
        if isinstance(spec, type):
            # Simple type validation
            return self._convert_value(value, spec, field_path)
        
        elif isinstance(spec, tuple) and len(spec) >= 2:
            # Type with constraint function (and optional constraints dict)
            field_type = spec[0]
            constraint_fn = spec[1]
            converted_value = self._convert_value(value, field_type, field_path)
            
            if not constraint_fn(converted_value):
                raise KvlValidationError(f"Value fails constraint check", field_path, converted_value)
            
            return converted_value
        
        elif isinstance(spec, dict):
            if self._is_constraint_spec(spec):
                # Complex constraint spec
                constraint_spec = self._parse_constraint_spec(spec)
                return self._validate_value_against_spec(value, constraint_spec, path)
            else:
                # Nested subschema - recursively validate
                if not isinstance(value, dict):
                    raise KvlTypeError(f"Expected dict for nested schema, got {type(value).__name__}", field_path, value)
                
                return self._validate_recursive(value, spec, path)
        
        else:
            raise KvlSchemaError(f"Invalid field specification: {spec}")
    
    def _check_required_fields(self, data: Dict[str, Any], subschema: Dict[str, Any], path: List[str]):
        """Check that all required fields in a subschema are present."""
        for field_name, field_spec in subschema.items():
            if self._is_field_required(field_spec):
                if field_name not in data:
                    field_path = '.'.join(path + [field_name])
                    raise KvlValidationError(f"Required field '{field_name}' is missing", field_path, None)
    
    def _is_field_required(self, field_spec: Any) -> bool:
        """Check if a field specification indicates the field is required."""
        if isinstance(field_spec, tuple) and len(field_spec) == 3:
            # This is a (type, constraint_fn, constraints_dict) tuple
            constraints_dict = field_spec[2]
            required_value = constraints_dict.get('required', self.required)
            # Convert string boolean values
            if isinstance(required_value, str):
                return required_value.lower() in ('true', 'yes', '1')
            return bool(required_value)
        elif isinstance(field_spec, tuple) and len(field_spec) == 2:
            # This is a (type, constraint_fn) tuple without constraints dict
            return self.required
        elif isinstance(field_spec, dict) and self._is_constraint_spec(field_spec):
            required_value = field_spec.get('required', self.required)
            if isinstance(required_value, str):
                return required_value.lower() in ('true', 'yes', '1')
            return bool(required_value)
        return self.required
    
    def _convert_value(self, value: Any, target_type: type, path: str) -> Any:
        """Convert a value to the target type."""
        
        # Handle list/array types
        if target_type == list:
            return self._convert_to_list(value, path)
        
        # Handle dict types  
        if target_type == dict:
            if isinstance(value, dict):
                return value
            else:
                raise KvlTypeError(f"Cannot convert {type(value).__name__} to dict", path, value)
        
        # Handle datetime
        if target_type == datetime:
            return self._convert_to_datetime(value, path)
        
        # Handle bool (special case since bool is subclass of int)
        if target_type == bool:
            return self._convert_to_bool(value, path)
        
        # Handle numeric types
        if target_type in (int, float):
            return self._convert_to_number(value, target_type, path)
        
        # Handle string
        if target_type == str:
            return str(value)
        
        # Try direct conversion for other types
        if isinstance(value, target_type):
            return value
        
        try:
            return target_type(value)
        except Exception as e:
            raise KvlTypeError(f"Cannot convert {type(value).__name__} to {target_type.__name__}: {e}", path, value)
    
    def _convert_to_list(self, value: Any, path: str) -> List[Any]:
        """Convert KVL dict format or list to Python list."""
        if isinstance(value, list):
            return value
        elif isinstance(value, dict):
            # KVL array format: {"item1": "", "item2": "", ...}
            items = []
            for key, val in value.items():
                if val == "" or val == {}:
                    items.append(key)
                else:
                    # If values aren't empty, treat as nested dict
                    return value
            return items
        elif isinstance(value, str):
            # Single string value for list field - treat as single-item list
            return [value]
        else:
            raise KvlTypeError(f"Cannot convert {type(value).__name__} to list", path, value)
    
    def _convert_to_bool(self, value: Any, path: str) -> bool:
        """Convert value to boolean."""
        if isinstance(value, bool):
            return value
        elif isinstance(value, str):
            lower_val = value.lower()
            if lower_val in ('true', 'yes', '1', 'on'):
                return True
            elif lower_val in ('false', 'no', '0', 'off'):
                return False
            else:
                raise KvlTypeError(f"Cannot convert '{value}' to bool", path, value)
        elif isinstance(value, (int, float)):
            return bool(value)
        else:
            raise KvlTypeError(f"Cannot convert {type(value).__name__} to bool", path, value)
    
    def _convert_to_number(self, value: Any, target_type: type, path: str) -> Union[int, float]:
        """Convert value to int or float."""
        if isinstance(value, target_type):
            return value
        elif isinstance(value, str):
            try:
                return target_type(value)
            except ValueError:
                raise KvlTypeError(f"Cannot convert '{value}' to {target_type.__name__}", path, value)
        elif isinstance(value, (int, float)):
            return target_type(value)
        else:
            raise KvlTypeError(f"Cannot convert {type(value).__name__} to {target_type.__name__}", path, value)
    
    def _convert_to_datetime(self, value: Any, path: str) -> datetime:
        """Convert value to datetime."""
        if isinstance(value, datetime):
            return value
        elif isinstance(value, str):
            # Try common datetime formats
            formats = [
                "%Y-%m-%d %H:%M:%S",
                "%Y-%m-%d",
                "%Y-%m-%dT%H:%M:%S",
                "%Y-%m-%dT%H:%M:%SZ"
            ]
            
            for fmt in formats:
                try:
                    return datetime.strptime(value, fmt)
                except ValueError:
                    continue
            
            raise KvlTypeError(f"Cannot parse datetime '{value}'", path, value)
        else:
            raise KvlTypeError(f"Cannot convert {type(value).__name__} to datetime", path, value)
    
    def serialize(self, data: Dict[str, Any], path: str = "") -> Dict[str, Any]:
        """
        Convert typed Python dict to KVL-compatible dict.
        
        Args:
            data: Typed Python dictionary to convert
            path: Current path for error reporting
            
        Returns:
            KVL-compatible dictionary
        """
        if not isinstance(data, dict):
            raise KvlTypeError(f"Expected dict for serialization, got {type(data).__name__}", path, data)
        
        result = {}
        
        for field_name, field_value in data.items():
            field_path = f"{path}.{field_name}" if path else field_name
            
            # Find appropriate field spec for serialization
            field_spec = self._find_field_spec_for_serialization(field_name, data)
            
            if field_spec:
                result[field_name] = self._serialize_value(field_value, field_spec, field_path)
            else:
                # Pass through unknown fields
                result[field_name] = field_value
        
        return result
    
    def _find_field_spec_for_serialization(self, field_name: str, context: Dict[str, Any]) -> Any:
        """Find appropriate field spec for serialization."""
        # For now, just look in root fields
        return self.fields.get(field_name)
    
    def _serialize_value(self, value: Any, field_spec: Any, path: str) -> Any:
        """Serialize a single value back to KVL format."""
        if isinstance(field_spec, type):
            target_type = field_spec
        elif isinstance(field_spec, tuple) and len(field_spec) >= 2:
            target_type = field_spec[0]
        elif isinstance(field_spec, dict) and self._is_constraint_spec(field_spec):
            target_type = self._parse_type_name(field_spec['type'])
        else:
            return value
        
        if target_type == list and isinstance(value, list):
            # Convert list back to KVL dict format
            return {str(item): {} for item in value}
        elif target_type == bool and isinstance(value, bool):
            return "true" if value else "false"
        elif target_type == datetime and isinstance(value, datetime):
            return value.strftime("%Y-%m-%d %H:%M:%S")
        elif target_type in (int, float, str):
            return str(value)
        else:
            return value
    
    def loads(self, kvl_text: str) -> Dict[str, Any]:
        """
        Load KVL text and validate with schema.
        
        Args:
            kvl_text: KVL text to parse
            
        Returns:
            Typed Python dictionary
        """
        raw_data = loads(kvl_text)
        return self.deserialize(raw_data)
    
    def load(self, file_or_path: FileOrPath) -> Dict[str, Any]:
        """
        Load KVL file and validate with schema.
        
        Args:
            file_or_path: File path or file-like object
            
        Returns:
            Typed Python dictionary
        """
        raw_data = load(file_or_path)
        return self.deserialize(raw_data)
    
    def dumps(self, data: Dict[str, Any], **options) -> str:
        """
        Serialize typed Python dict and convert to KVL text.
        
        Args:
            data: Typed Python dictionary
            options: Additional options for KVL serialization
            
        Returns:
            KVL text string
        """
        kvl_data = self.serialize(data)
        return dumps(kvl_data, **options)
    
    def dump(self, data: Dict[str, Any], file_or_path: FileOrPath, **options) -> None:
        """
        Serialize typed Python dict and write to KVL file.
        
        Args:
            data: Typed Python dictionary
            file_or_path: File path or file-like object
            options: Additional options for KVL serialization
        """
        kvl_data = self.serialize(data)
        dump(kvl_data, file_or_path, **options)
    
    @classmethod
    def from_kvl(cls, kvl_text: str) -> 'Schema':
        """
        Load schema definition from KVL text.
        
        Args:
            kvl_text: KVL text containing schema definition
            
        Returns:
            Schema instance
            
        Raises:
            KvlSchemaError: If schema definition is invalid
        """
        return cls._parse_schema_kvl(kvl_text)
    
    @classmethod
    def from_file(cls, file_path: str) -> 'Schema':
        """
        Load schema definition from KVL schema file.
        
        Args:
            file_path: Path to .kvl.schema file
            
        Returns:
            Schema instance
            
        Raises:
            KvlSchemaError: If schema file is invalid
        """
        kvl_text = handle_read(file_path)
        return cls.from_kvl(kvl_text)
    
    @classmethod
    def _parse_schema_kvl(cls, kvl_text: str) -> 'Schema':
        """Parse KVL schema definition into field specifications."""
        # Parse the KVL
        try:
            raw_data = loads(kvl_text)
        except Exception as e:
            raise KvlSchemaError(f"Failed to parse schema KVL: {e}") from e
        
        # Parse header for configuration
        open_mode, required_mode = cls._parse_schema_header(kvl_text)
        
        # Extract schema fields
        fields = {}
        
        for field_name, field_def in raw_data.items():
            # Skip metadata fields and comments
            if cls._is_metadata_field(field_name, field_def):
                continue
                
            field_spec = cls._parse_field_definition(field_name, field_def, raw_data)
            if field_spec is not None:
                fields[field_name] = field_spec
        
        return cls(fields, open=open_mode, required=required_mode)
    
    @classmethod
    def _parse_schema_header(cls, kvl_text: str) -> Tuple[bool, bool]:
        """Parse schema header for open/closed and required/optional settings."""
        lines = kvl_text.strip().split('\n')
        open_mode = True  # Default
        required_mode = False  # Default
        
        for line in lines[:5]:  # Check first few lines for headers
            line = line.strip()
            if line.startswith('#') and 'kvl' in line.lower() and 'schema' in line.lower():
                if 'closed' in line.lower():
                    open_mode = False
                if 'required' in line.lower():
                    required_mode = True
                break
        
        return open_mode, required_mode
    
    @classmethod
    def _is_metadata_field(cls, field_name: str, field_def: Any) -> bool:
        """Check if a field is metadata (header, comment, etc.) and should be skipped."""
        # Skip KVL headers: "#= kvl 1.0 schema" becomes field_name="#"
        if field_name.startswith('#'):
            return True
        
        # Skip KVL comments: "/= comment" becomes field_name="/"  
        if field_name.startswith('/'):
            return True
        
        return False
    
    @classmethod
    def _parse_field_definition(cls, field_name: str, field_def: Any, schema_data: Dict[str, Any]) -> Optional[Any]:
        """Parse a single field definition from schema data."""
        
        # Simple type definition: "name = string"
        if isinstance(field_def, str):
            base_type = cls._parse_type_name(field_def)
            return base_type
        
        # Complex definition: dict
        elif isinstance(field_def, dict):
            # Check if it's a constraint spec or nested schema
            if 'type' in field_def:
                # Constraint spec: "name = { type = string, min-length = 3, ... }"
                base_type = cls._parse_type_name(field_def['type'])
                constraints = {k: v for k, v in field_def.items() if k != 'type'}
                
                if constraints:
                    constraint_fn = cls._build_constraint_function(constraints)
                    return (base_type, constraint_fn, constraints)
                else:
                    return base_type
            else:
                # Nested schema: "server = { host = string, port = int }"
                nested_fields = {}
                for nested_name, nested_def in field_def.items():
                    nested_spec = cls._parse_field_definition(nested_name, nested_def, field_def)
                    if nested_spec is not None:
                        nested_fields[nested_name] = nested_spec
                return nested_fields
        
        else:
            raise KvlSchemaError(f"Invalid field definition for '{field_name}': {field_def}")
    
    @classmethod
    def _parse_type_name(cls, type_name: str) -> type:
        """Convert string type name to Python type."""
        type_mapping = {
            'string': str,
            'str': str,
            'integer': int,
            'int': int,
            'float': float,
            'number': float,
            'boolean': bool,
            'bool': bool,
            'list': list,
            'array': list,
            'object': dict,
            'dict': dict,
            'datetime': datetime,
            'date': datetime,
        }
        
        if type_name in type_mapping:
            return type_mapping[type_name]
        else:
            raise KvlSchemaError(f"Unknown type: {type_name}")
    
    @classmethod
    def _build_constraint_function(cls, constraints: Dict[str, Any]) -> Callable[[Any], bool]:
        """Build a constraint validation function from constraint definitions."""
        
        def validate_constraints(value: Any) -> bool:
            # Numeric constraints
            if 'min' in constraints:
                if isinstance(value, (int, float)) and value < float(constraints['min']):
                    return False
            
            if 'max' in constraints:
                if isinstance(value, (int, float)) and value > float(constraints['max']):
                    return False
            
            # String constraints (support both underscore and hyphen)
            min_length_key = 'min-length' if 'min-length' in constraints else 'min_length'
            if min_length_key in constraints:
                if isinstance(value, str) and len(value) < int(constraints[min_length_key]):
                    return False
            
            max_length_key = 'max-length' if 'max-length' in constraints else 'max_length'
            if max_length_key in constraints:
                if isinstance(value, str) and len(value) > int(constraints[max_length_key]):
                    return False
            
            if 'pattern' in constraints:
                if isinstance(value, str):
                    try:
                        if not re.match(constraints['pattern'], value):
                            return False
                    except Exception as e:
                        raise KvlSchemaError(f"Invalid regex pattern '{constraints['pattern']}': {e}") from e
            
            if 'enum' in constraints:
                enum_values = constraints['enum']
                if isinstance(enum_values, str):
                    # Parse comma-separated values
                    enum_list = [v.strip() for v in enum_values.split(',')]
                elif isinstance(enum_values, list):
                    enum_list = enum_values
                else:
                    enum_list = [enum_values]
                
                # Convert enum values to match the value type for comparison
                if isinstance(value, (int, float)):
                    try:
                        enum_list = [float(v) if '.' in str(v) else int(v) for v in enum_list]
                    except ValueError:
                        pass  # Keep original values if conversion fails
                elif isinstance(value, str):
                    enum_list = [str(v) for v in enum_list]
                
                if value not in enum_list:
                    return False
            
            # List constraints (support both underscore and hyphen)
            min_items_key = 'min-items' if 'min-items' in constraints else 'min_items'
            if min_items_key in constraints:
                if isinstance(value, list) and len(value) < int(constraints[min_items_key]):
                    return False
            
            max_items_key = 'max-items' if 'max-items' in constraints else 'max_items'
            if max_items_key in constraints:
                if isinstance(value, list) and len(value) > int(constraints[max_items_key]):
                    return False
            
            return True
        
        return validate_constraints