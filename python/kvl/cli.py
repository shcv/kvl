#!/usr/bin/env python3
"""Command line interface for KVL (Key-Value Language).

Based on the original Categorical Configuration Language (CCL) by Dmitrii Kovanikov (chshersh).
See: https://c-cube.github.io/ocaml-ccl/ and https://github.com/c-cube/ocaml-ccl
"""

import argparse
import json
import sys
from pathlib import Path
from typing import Any, Dict, Optional

import kvl
from kvl.schema import Schema
from kvl.errors import KvlError, KvlSchemaError, KvlValidationError


def convert_command(args) -> None:
    """Convert between KVL and other formats."""
    input_path = Path(args.input)
    
    if not input_path.exists():
        print(f"Error: Input file '{input_path}' does not exist", file=sys.stderr)
        sys.exit(1)
    
    # Create config if separator specified (only for output override)
    output_config = None
    if hasattr(args, 'separator') and args.separator:
        output_config = kvl.auto_config_for_separator(args.separator)
    
    try:
        # Determine input format
        if input_path.suffix.lower() == '.json':
            with open(input_path, 'r') as f:
                data = json.load(f)
        elif input_path.suffix.lower() == '.kvl':
            with open(input_path, 'r') as f:
                data = kvl.load(f)  # Let KVL auto-detect separator from header
        else:
            print(f"Error: Unsupported input format '{input_path.suffix}'", file=sys.stderr)
            sys.exit(1)
        
        # Determine output format and write
        if args.output:
            output_path = Path(args.output)
            if output_path.suffix.lower() == '.json':
                with open(output_path, 'w') as f:
                    json.dump(data, f, indent=2)
            elif output_path.suffix.lower() == '.kvl':
                with open(output_path, 'w') as f:
                    kvl.dump(data, f, config=output_config, include_header=bool(output_config))
            else:
                print(f"Error: Unsupported output format '{output_path.suffix}'", file=sys.stderr)
                sys.exit(1)
            print(f"Converted '{input_path}' to '{output_path}'")
        else:
            # Output to stdout - determine format from input extension or default to KVL
            if input_path.suffix.lower() == '.json':
                # Input is JSON, output KVL to stdout
                kvl.dump(data, sys.stdout, config=output_config, include_header=bool(output_config))
            else:
                # Input is KVL, output JSON to stdout
                json.dump(data, sys.stdout, indent=2)
                print()  # Add newline after JSON output
        
    except kvl.KvlError as e:
        print(f"KVL Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def validate_command(args) -> None:
    """Validate a KVL file with optional schema validation."""
    file_path = Path(args.file)
    
    if not file_path.exists():
        print(f"Error: File '{file_path}' does not exist", file=sys.stderr)
        sys.exit(1)
    
    # Create config if separator specified
    config = None
    if hasattr(args, 'separator') and args.separator:
        config = kvl.auto_config_for_separator(args.separator)
    
    try:
        # Basic KVL syntax validation
        with open(file_path, 'r') as f:
            data = kvl.load(f, config)
        print(f"'{file_path}' has valid KVL syntax")
        
        # Schema validation if schema file provided
        if hasattr(args, 'schema') and args.schema:
            schema_path = Path(args.schema)
            if not schema_path.exists():
                print(f"Error: Schema file '{schema_path}' does not exist", file=sys.stderr)
                sys.exit(1)
            
            try:
                schema = Schema.from_file(str(schema_path))
                schema.validate(data)
                print(f"'{file_path}' is valid according to schema '{schema_path}'")
            except KvlSchemaError as e:
                print(f"Schema error: {e}", file=sys.stderr)
                sys.exit(1)
            except KvlValidationError as e:
                print(f"Validation failed: {e}", file=sys.stderr)
                sys.exit(1)
                
    except kvl.KvlError as e:
        print(f"Validation failed: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def schema_convert_command(args) -> None:
    """Convert KVL file through schema validation with type conversion."""
    input_path = Path(args.input)
    schema_path = Path(args.schema)
    
    if not input_path.exists():
        print(f"Error: Input file '{input_path}' does not exist", file=sys.stderr)
        sys.exit(1)
    
    if not schema_path.exists():
        print(f"Error: Schema file '{schema_path}' does not exist", file=sys.stderr)
        sys.exit(1)
    
    try:
        # Load schema
        schema = Schema.from_file(str(schema_path))
        
        # Load and validate data with type conversion
        validated_data = schema.load(str(input_path))
        
        # Output based on format
        if args.output:
            output_path = Path(args.output)
            if output_path.suffix.lower() == '.json':
                with open(output_path, 'w') as f:
                    json.dump(validated_data, f, indent=2, default=str)
            elif output_path.suffix.lower() == '.kvl':
                schema.dump(validated_data, str(output_path))
            else:
                print(f"Error: Unsupported output format '{output_path.suffix}'", file=sys.stderr)
                sys.exit(1)
            print(f"Schema-validated data written to '{output_path}'")
        else:
            # Output to stdout as JSON by default
            json.dump(validated_data, sys.stdout, indent=2, default=str)
            print()
            
    except KvlSchemaError as e:
        print(f"Schema error: {e}", file=sys.stderr)
        sys.exit(1)
    except KvlValidationError as e:
        print(f"Validation failed: {e}", file=sys.stderr)
        sys.exit(1)
    except kvl.KvlError as e:
        print(f"KVL error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def format_command(args) -> None:
    """Format a KVL file."""
    file_path = Path(args.file)
    
    if not file_path.exists():
        print(f"Error: File '{file_path}' does not exist", file=sys.stderr)
        sys.exit(1)
    
    try:
        # First, try to detect the original format
        with open(file_path, 'r') as f:
            file_content = f.read()
        
        # Parse header to get original config
        original_config = kvl.parse_header(file_content)
        
        # Override with command line separator if specified
        if hasattr(args, 'separator') and args.separator:
            config = kvl.auto_config_for_separator(args.separator)
        else:
            config = original_config
        
        # Parse the data
        data = kvl.loads(file_content)
        
        if args.output:
            output_path = Path(args.output)
            with open(output_path, 'w') as f:
                kvl.dump(data, f, config=config, include_header=bool(config))
            print(f"Formatted KVL written to '{output_path}'")
        elif args.in_place:
            # Format in place
            with open(file_path, 'w') as f:
                kvl.dump(data, f, config=config, include_header=bool(config))
            print(f"Formatted '{file_path}' in place")
        else:
            # Output to stdout by default
            kvl.dump(data, sys.stdout, config=config, include_header=bool(config))
            
    except kvl.KvlError as e:
        print(f"Format failed: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def parse_raw_command(args) -> None:
    """Parse a KVL file to raw categorical JSON."""
    input_path = Path(args.input)

    if not input_path.exists():
        print(f"Error: Input file '{input_path}' does not exist", file=sys.stderr)
        sys.exit(1)

    try:
        with open(input_path, 'r') as f:
            text = f.read()
        data = kvl.parse(text)
        json.dump(data, sys.stdout, indent=2, ensure_ascii=False)
        print()
    except kvl.KvlError as e:
        print(f"KVL Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def serialize_command(args) -> None:
    """Serialize JSON from stdin to KVL text on stdout."""
    config = None
    if hasattr(args, 'separator') and args.separator:
        config = kvl.auto_config_for_separator(args.separator)

    try:
        data = json.load(sys.stdin)
        kvl.dump(data, sys.stdout, config=config, include_header=bool(config))
    except json.JSONDecodeError as e:
        print(f"Invalid JSON input: {e}", file=sys.stderr)
        sys.exit(1)
    except kvl.KvlError as e:
        print(f"KVL Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def merge_command(args) -> None:
    """Merge multiple KVL files using categorical merge."""
    merged_data: Dict[str, Any] = {}

    for file_path in args.files:
        path = Path(file_path)
        if not path.exists():
            print(f"Error: File '{path}' does not exist", file=sys.stderr)
            sys.exit(1)

        try:
            with open(path, 'r') as f:
                text = f.read()
            data = kvl.parse(text)
            merged_data = kvl.merge(merged_data, data)
        except kvl.KvlError as e:
            print(f"Merge failed for '{path}': {e}", file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            print(f"Error processing '{path}': {e}", file=sys.stderr)
            sys.exit(1)

    try:
        compacted = kvl.compact(merged_data)
        if args.output:
            output_path = Path(args.output)
            with open(output_path, 'w') as f:
                json.dump(compacted, f, indent=2, ensure_ascii=False)
            print(f"Merged output written to '{output_path}'")
        else:
            json.dump(compacted, sys.stdout, indent=2, ensure_ascii=False)
            print()
    except kvl.KvlError as e:
        print(f"Output failed: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def main() -> None:
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        prog='kvl',
        description='KVL (Key-Value Language) command line tool'
    )
    parser.add_argument('--version', action='version', version=f'kvl {kvl.__version__}')
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Convert command
    convert_parser = subparsers.add_parser('convert', help='Convert between KVL and other formats')
    convert_parser.add_argument('input', help='Input file (supports .kvl, .json)')
    convert_parser.add_argument('output', nargs='?', help='Output file (supports .kvl, .json) - defaults to stdout')
    convert_parser.add_argument('-s', '--separator', help='KVL separator to use (e.g., =, :, ->, :=)')
    convert_parser.set_defaults(func=convert_command)
    
    # Validate command
    validate_parser = subparsers.add_parser('validate', help='Validate a KVL file')
    validate_parser.add_argument('file', help='KVL file to validate')
    validate_parser.add_argument('-s', '--separator', help='KVL separator to use (e.g., =, :, ->, :=)')
    validate_parser.add_argument('--schema', help='Schema file for validation (.schema.kvl)')
    validate_parser.set_defaults(func=validate_command)
    
    # Format command
    format_parser = subparsers.add_parser('format', help='Format a KVL file')
    format_parser.add_argument('file', help='KVL file to format')
    format_parser.add_argument('-o', '--output', help='Output file (default: stdout)')
    format_parser.add_argument('-i', '--in-place', action='store_true', help='Format file in place')
    format_parser.add_argument('-s', '--separator', help='KVL separator to use (e.g., =, :, ->, :=)')
    format_parser.set_defaults(func=format_command)
    
    # Parse-raw command
    parse_raw_parser = subparsers.add_parser('parse-raw', help='Parse KVL file to raw categorical JSON')
    parse_raw_parser.add_argument('input', help='Input KVL file')
    parse_raw_parser.set_defaults(func=parse_raw_command)

    # Serialize command
    serialize_parser = subparsers.add_parser('serialize', help='Serialize JSON from stdin to KVL on stdout')
    serialize_parser.add_argument('-s', '--separator', help='KVL separator to use (e.g., =, :, ->, :=)')
    serialize_parser.set_defaults(func=serialize_command)

    # Merge command
    merge_parser = subparsers.add_parser('merge', help='Merge multiple KVL files')
    merge_parser.add_argument('files', nargs='+', help='KVL files to merge')
    merge_parser.add_argument('-o', '--output', help='Output file (default: stdout)')
    merge_parser.set_defaults(func=merge_command)
    
    # Schema convert command
    schema_parser = subparsers.add_parser('schema-convert', help='Convert KVL file with schema validation and type conversion')
    schema_parser.add_argument('input', help='Input KVL file')
    schema_parser.add_argument('schema', help='Schema file (.schema.kvl)')
    schema_parser.add_argument('-o', '--output', help='Output file (.json or .kvl) - defaults to JSON stdout')
    schema_parser.set_defaults(func=schema_convert_command)
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        sys.exit(1)
    
    args.func(args)


if __name__ == '__main__':
    main()