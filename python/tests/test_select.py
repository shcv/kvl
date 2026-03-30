"""Tests for select() filter in KVQ hybrid query engine."""

import pytest
import kvl
from kvl.query_hybrid import (
    execute, parse_query,
    KvqError, KvqParseError, KvqPathError, KvqTypeError,
)


def q(query_str, text):
    return execute(query_str, kvl.loads(text))


SERVERS_KVL = """\
servers =
  web1 =
    name = web1
    port = 80
  web2 =
    name = web2
    port = 8080
  api1 =
    name = api1
    port = 443
"""

TAGS_KVL = """\
tags = web
tags = api
tags = production
"""

SERVERS_CONFIG_KVL = """\
servers =
  web1 =
    name = web1
    port = 80
    config =
      ssl = false
  web2 =
    name = web2
    port = 443
    config =
      ssl = true
"""


# ---------------------------------------------------------------------------
# Parser tests
# ---------------------------------------------------------------------------

class TestSelectParse:
    def test_parse_select_gt(self):
        ops = parse_query('servers[] | select(.port > 80)')
        assert ops[-1][0] == 'select'
        path_parts, op, value = ops[-1][1]
        assert path_parts == ['port']
        assert op == '>'
        assert value == '80'

    def test_parse_select_eq(self):
        ops = parse_query('tags[] | select(. == api)')
        assert ops[-1][0] == 'select'
        path_parts, op, value = ops[-1][1]
        assert path_parts == []
        assert op == '=='
        assert value == 'api'

    def test_parse_select_neq(self):
        ops = parse_query('servers[] | select(.name != web1)')
        assert ops[-1][0] == 'select'
        path_parts, op, value = ops[-1][1]
        assert path_parts == ['name']
        assert op == '!='

    def test_parse_select_nested_path(self):
        ops = parse_query('servers[] | select(.config.ssl == true)')
        assert ops[-1][0] == 'select'
        path_parts, op, value = ops[-1][1]
        assert path_parts == ['config', 'ssl']
        assert op == '=='
        assert value is True

    def test_parse_select_lte(self):
        ops = parse_query('items[] | select(.score <= 10)')
        assert ops[-1][1][1] == '<='

    def test_parse_select_gte(self):
        ops = parse_query('items[] | select(.score >= 5)')
        assert ops[-1][1][1] == '>='

    def test_parse_select_lt(self):
        ops = parse_query('items[] | select(.val < 100)')
        assert ops[-1][1][1] == '<'

    def test_parse_select_missing_lparen(self):
        with pytest.raises(KvqParseError, match="Expected '\\('"):
            parse_query('tags[] | select .x == 1')

    def test_parse_select_missing_rparen(self):
        with pytest.raises(KvqParseError, match="Expected '\\)'"):
            parse_query('tags[] | select(.x == 1')

    def test_parse_select_missing_dot(self):
        with pytest.raises(KvqParseError, match="must start with '\\.'"):
            parse_query('tags[] | select(x == 1)')

    def test_parse_select_missing_operator(self):
        with pytest.raises(KvqParseError, match="comparison operator"):
            parse_query('tags[] | select(.x)')


# ---------------------------------------------------------------------------
# Execution tests
# ---------------------------------------------------------------------------

class TestSelectExec:
    def test_select_port_gt(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.port > 80)', data)
        # Returns servers with port > 80 (numeric comparison: 8080 and 443 > 80)
        assert isinstance(result, list)
        names = [r['name'] for r in result]
        assert 'web1' not in names
        assert 'web2' in names
        assert 'api1' in names

    def test_select_port_eq(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.port == 80)', data)
        assert isinstance(result, dict)  # single result unwrapped
        assert result['name'] == 'web1'

    def test_select_tag_identity(self):
        data = kvl.loads(TAGS_KVL)
        result = execute('tags[] | select(. == api)', data)
        assert result == 'api'

    def test_select_tag_neq(self):
        data = kvl.loads(TAGS_KVL)
        result = execute('tags[] | select(. != api)', data)
        assert isinstance(result, list)
        assert 'api' not in result
        assert 'web' in result
        assert 'production' in result

    def test_select_name_neq(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.name != web1)', data)
        assert isinstance(result, list)
        names = [r['name'] for r in result]
        assert 'web1' not in names
        assert 'web2' in names

    def test_select_nested_config_ssl(self):
        data = kvl.loads(SERVERS_CONFIG_KVL)
        result = execute('servers[] | select(.config.ssl == true)', data)
        # Only web2 has ssl = true
        assert isinstance(result, dict)
        assert result['name'] == 'web2'

    def test_select_nested_ssl_false(self):
        data = kvl.loads(SERVERS_CONFIG_KVL)
        result = execute('servers[] | select(.config.ssl == false)', data)
        assert isinstance(result, dict)
        assert result['name'] == 'web1'

    def test_select_no_match_returns_empty(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.port > 9999)', data)
        assert result == []

    def test_select_all_match(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.port > 0)', data)
        assert isinstance(result, list)
        assert len(result) == 3

    def test_select_numeric_coercion(self):
        # String "100" and "80" should compare numerically
        data = {'items': [{'val': '100'}, {'val': '80'}, {'val': '50'}]}
        result = execute('items[] | select(.val > 80)', data)
        assert isinstance(result, dict)
        assert result['val'] == '100'

    def test_select_string_comparison(self):
        # Non-numeric strings compare lexicographically
        data = {'words': [{'w': 'apple'}, {'w': 'banana'}, {'w': 'cherry'}]}
        result = execute('words[] | select(.w == banana)', data)
        assert result == {'w': 'banana'}

    def test_select_chained_with_get(self):
        # After select, can continue getting fields
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.port > 80)', data)
        # Result is list of dicts; further nav would need another pipe
        assert all('name' in r for r in result)

    def test_select_lt(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.port < 443)', data)
        assert isinstance(result, dict)
        assert result['name'] == 'web1'

    def test_select_lte(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.port <= 80)', data)
        assert isinstance(result, dict)
        assert result['name'] == 'web1'

    def test_select_gte(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[] | select(.port >= 443)', data)
        assert isinstance(result, list)
        names = [r['name'] for r in result]
        assert 'web2' in names
        assert 'api1' in names


# ---------------------------------------------------------------------------
# Error cases
# ---------------------------------------------------------------------------

class TestSelectErrors:
    def test_select_on_scalar_raises(self):
        # select on a scalar (non-iterable after iter) — path not found
        with pytest.raises((KvqTypeError, KvqPathError)):
            execute('tags[] | select(.port > 80)', kvl.loads(TAGS_KVL))

    def test_select_missing_field_raises(self):
        data = {'items': [{'a': '1'}, {'b': '2'}]}
        with pytest.raises(KvqPathError):
            execute('items[] | select(.missing == 1)', data)

    def test_select_invalid_syntax_no_dot(self):
        with pytest.raises(KvqParseError):
            parse_query('items[] | select(port > 80)')

    def test_select_invalid_syntax_empty(self):
        with pytest.raises(KvqParseError):
            parse_query('items[] | select()')
