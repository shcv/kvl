"""Tests for quoted string literal support in KVQ select() predicates."""

import pytest
import kvl
from kvl.query import execute, parse_query, KvqParseError


def q(query_str, data_dict):
    return execute(query_str, data_dict)


ITEMS = {
    'items': [
        {'name': 'web 1', 'tag': 'prod'},
        {'name': 'web 2', 'tag': 'dev'},
        {'name': 'api-server', 'tag': 'prod'},
    ]
}

SPECIAL = {
    'entries': [
        {'key': 'x=y', 'val': 'a>b'},
        {'key': 'a==b', 'val': 'c<d'},
        {'key': 'normal', 'val': 'plain'},
    ]
}


# ---------------------------------------------------------------------------
# Parser: quoted string tokens
# ---------------------------------------------------------------------------

class TestStringLiteralParse:
    def test_quoted_value_parsed(self):
        ops = parse_query('items[] | select(.name == "web 1")')
        path_parts, op, value = ops[-1][1]
        assert value == 'web 1'
        assert op == '=='

    def test_empty_string(self):
        ops = parse_query('items[] | select(.name == "")')
        assert ops[-1][1][2] == ''

    def test_string_with_equals(self):
        ops = parse_query('entries[] | select(.key == "x=y")')
        assert ops[-1][1][2] == 'x=y'

    def test_string_with_operators(self):
        ops = parse_query('entries[] | select(.val == "a>b")')
        assert ops[-1][1][2] == 'a>b'

    def test_string_with_escaped_quote(self):
        ops = parse_query('items[] | select(.name == "say \\"hi\\"")')
        assert ops[-1][1][2] == 'say "hi"'

    def test_string_with_escaped_backslash(self):
        ops = parse_query('items[] | select(.path == "C:\\\\dir")')
        assert ops[-1][1][2] == 'C:\\dir'

    def test_string_with_spaces(self):
        ops = parse_query('items[] | select(.name == "hello world")')
        assert ops[-1][1][2] == 'hello world'

    def test_string_with_lt_gt(self):
        ops = parse_query('entries[] | select(.val == "c<d")')
        assert ops[-1][1][2] == 'c<d'

    def test_neq_quoted(self):
        ops = parse_query('items[] | select(.tag != "dev")')
        assert ops[-1][1][1] == '!='
        assert ops[-1][1][2] == 'dev'


# ---------------------------------------------------------------------------
# Execution: quoted string matching
# ---------------------------------------------------------------------------

class TestStringLiteralExec:
    def test_match_name_with_space(self):
        result = q('items[] | select(.name == "web 1")', ITEMS)
        assert result == {'name': 'web 1', 'tag': 'prod'}

    def test_no_match_quoted(self):
        result = q('items[] | select(.name == "nonexistent")', ITEMS)
        assert result == []

    def test_match_multiple_quoted(self):
        result = q('items[] | select(.tag == "prod")', ITEMS)
        assert isinstance(result, list)
        assert len(result) == 2
        names = [r['name'] for r in result]
        assert 'web 1' in names
        assert 'api-server' in names

    def test_neq_quoted(self):
        result = q('items[] | select(.tag != "prod")', ITEMS)
        assert result == {'name': 'web 2', 'tag': 'dev'}

    def test_match_key_with_equals(self):
        result = q('entries[] | select(.key == "x=y")', SPECIAL)
        assert result == {'key': 'x=y', 'val': 'a>b'}

    def test_match_val_with_gt(self):
        result = q('entries[] | select(.val == "a>b")', SPECIAL)
        assert result == {'key': 'x=y', 'val': 'a>b'}

    def test_match_val_with_lt(self):
        result = q('entries[] | select(.val == "c<d")', SPECIAL)
        assert result == {'key': 'a==b', 'val': 'c<d'}

    def test_empty_string_match(self):
        data = {'items': [{'x': ''}, {'x': 'hello'}]}
        result = q('items[] | select(.x == "")', data)
        assert result == {'x': ''}

    def test_string_with_double_equals_in_value(self):
        result = q('entries[] | select(.key == "a==b")', SPECIAL)
        assert result == {'key': 'a==b', 'val': 'c<d'}

    def test_unquoted_still_works(self):
        # Unquoted values should still work alongside quoted
        result = q('items[] | select(.tag == prod)', ITEMS)
        assert isinstance(result, list)
        assert len(result) == 2
