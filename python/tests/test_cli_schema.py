"""Tests for schema-related CLI commands."""

import json
from types import SimpleNamespace

import pytest

from kvl.cli import schema_convert_command, validate_command


def test_validate_command_json_success(tmp_path, capsys):
    """Validate emits JSON diagnostics on success when requested."""
    input_path = tmp_path / "config.kvl"
    schema_path = tmp_path / "config.schema.kvl"

    input_path.write_text(
        "server =\n"
        "    host = localhost\n"
        "    port = 8080\n"
    )
    schema_path.write_text(
        "#= kvl 1.0 schema open optional\n"
        "\n"
        "server =\n"
        "    host = string\n"
        "    port = int\n"
    )

    args = SimpleNamespace(
        file=str(input_path),
        schema=str(schema_path),
        separator=None,
        json=True,
    )

    validate_command(args)
    output = capsys.readouterr()

    payload = json.loads(output.out)
    assert payload == {
        "ok": True,
        "command": "validate",
        "file": str(input_path),
        "schema": str(schema_path),
        "syntax_valid": True,
        "schema_valid": True,
    }
    assert output.err == ""


def test_validate_command_json_schema_failure(tmp_path, capsys):
    """Validate emits structured schema errors when requested."""
    input_path = tmp_path / "config.kvl"
    schema_path = tmp_path / "config.schema.kvl"

    input_path.write_text("unknown = value\n")
    schema_path.write_text(
        "#= kvl 1.0 schema closed optional\n"
        "\n"
        "known = int\n"
    )

    args = SimpleNamespace(
        file=str(input_path),
        schema=str(schema_path),
        separator=None,
        json=True,
    )

    with pytest.raises(SystemExit) as excinfo:
        validate_command(args)

    assert excinfo.value.code == 1

    output = capsys.readouterr()
    payload = json.loads(output.err)
    assert payload["ok"] is False
    assert payload["command"] == "validate"
    assert payload["file"] == str(input_path)
    assert payload["schema"] == str(schema_path)
    assert payload["syntax_valid"] is True
    assert payload["schema_valid"] is False
    assert payload["error"]["stage"] == "schema"
    assert payload["error"]["type"] == "KvlValidationError"
    assert payload["error"]["path"] == "unknown"
    assert "Unknown field 'unknown'" in payload["error"]["message"]
    assert output.out == ""


def test_schema_convert_command_json_success_with_output_file(tmp_path, capsys):
    """schema-convert emits JSON diagnostics without changing the output file."""
    input_path = tmp_path / "config.kvl"
    schema_path = tmp_path / "config.schema.kvl"
    output_path = tmp_path / "converted.json"

    input_path.write_text(
        "server =\n"
        "    host = localhost\n"
        "    port = 8080\n"
    )
    schema_path.write_text(
        "#= kvl 1.0 schema open optional\n"
        "\n"
        "server =\n"
        "    host = string\n"
        "    port = int\n"
    )

    args = SimpleNamespace(
        input=str(input_path),
        schema=str(schema_path),
        output=str(output_path),
        json=True,
    )

    schema_convert_command(args)
    output = capsys.readouterr()

    payload = json.loads(output.out)
    assert payload == {
        "ok": True,
        "command": "schema-convert",
        "input": str(input_path),
        "schema": str(schema_path),
        "output": str(output_path),
        "output_format": "json",
    }
    assert output.err == ""
    assert json.loads(output_path.read_text()) == {
        "server": {
            "host": "localhost",
            "port": 8080,
        }
    }
