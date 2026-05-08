"""Tests for the KVQ query engine.

Tests cover:
- Parse op list
- Basic path navigation
- Array operations: index, slice, iterate
- Pipe functions: length, keys, min, max, sum
- Edge cases: unicode, hyphenated/escaped keys, multiline values, empty structures
- Multi-file merge
- Conformance against fixtures
- Query-engine candidate regressions
"""

import os
import pytest
import kvl
from kvl.query import (
    execute, query, parse_query,
    KvqError, KvqParseError, KvqPathError, KvqTypeError, KvqIndexError,
)

FIXTURES = os.path.join(os.path.dirname(__file__), '../../fixtures')


def q(query_str, text):
    """Helper: parse KVL text, execute query."""
    return execute(query_str, kvl.loads(text))


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------

class TestParseQuery:
    def test_dot_root(self):
        assert parse_query('.') == [('dot',)]

    def test_simple_ident(self):
        assert parse_query('name') == [('get', 'name')]

    def test_nested(self):
        assert parse_query('server.host') == [('get', 'server'), ('get', 'host')]

    def test_deep(self):
        assert parse_query('a.b.c') == [('get', 'a'), ('get', 'b'), ('get', 'c')]

    def test_iter(self):
        assert parse_query('tags[]') == [('get', 'tags'), ('iter',)]

    def test_index(self):
        assert parse_query('tags[0]') == [('get', 'tags'), ('index', 0)]

    def test_negative_index(self):
        assert parse_query('tags[-1]') == [('get', 'tags'), ('index', -1)]

    def test_slice_both(self):
        assert parse_query('tags[1:3]') == [('get', 'tags'), ('slice', 1, 3)]

    def test_slice_open_end(self):
        assert parse_query('tags[1:]') == [('get', 'tags'), ('slice', 1, None)]

    def test_slice_open_start(self):
        assert parse_query('tags[:2]') == [('get', 'tags'), ('slice', None, 2)]

    def test_slice_open_both(self):
        assert parse_query('tags[:]') == [('get', 'tags'), ('slice', None, None)]

    def test_pipe_function(self):
        assert parse_query('tags[] | length') == [
            ('get', 'tags'), ('iter',), ('apply', 'length')
        ]

    def test_dot_keys(self):
        assert parse_query('. | keys') == [('dot',), ('apply', 'keys')]

    def test_iter_then_get(self):
        assert parse_query('servers[].name') == [
            ('get', 'servers'), ('iter',), ('get', 'name')
        ]

    def test_hyphenated_key(self):
        assert parse_query('rate-limit') == [('get', 'rate-limit')]

    def test_equals_in_key(self):
        assert parse_query('x=y') == [('get', 'x=y')]

    def test_whitespace_ignored(self):
        assert parse_query('  tags [ ] | length  ') == [
            ('get', 'tags'), ('iter',), ('apply', 'length')
        ]

    def test_dot_prefix(self):
        assert parse_query('.server.host') == [
            ('dot',), ('get', 'server'), ('get', 'host')
        ]

    def test_empty_raises(self):
        with pytest.raises(KvqParseError):
            parse_query('')

    def test_unknown_function_raises(self):
        with pytest.raises(KvqParseError, match='Unknown function'):
            parse_query('tags | sort')

    def test_unclosed_bracket_raises(self):
        with pytest.raises(KvqParseError):
            parse_query('tags[')

    def test_invalid_bracket_content_raises(self):
        with pytest.raises(KvqParseError):
            parse_query('tags[abc]')

    def test_multiple_pipe_functions(self):
        ops = parse_query('scores | max')
        assert ops[-1] == ('apply', 'max')


# ---------------------------------------------------------------------------
# Basic path navigation
# ---------------------------------------------------------------------------

SERVER_KVL = "server =\n  host = localhost\n  port = 8080\n  ssl = true\n"

class TestBasicPath:
    def test_scalar(self):
        assert q('name', 'name = Alice Smith\n') == 'Alice Smith'

    def test_nested(self):
        assert q('server.host', SERVER_KVL) == 'localhost'
        assert q('server.port', SERVER_KVL) == '8080'

    def test_missing_key_raises(self):
        with pytest.raises(KvqPathError):
            q('server.missing', SERVER_KVL)

    def test_wrong_type_raises(self):
        with pytest.raises(KvqTypeError):
            q('server.host.sub', SERVER_KVL)

    def test_dot_identity(self):
        data = kvl.loads('name = test\nport = 8080\n')
        result = execute('.', data)
        assert result == {'name': 'test', 'port': '8080'}

    def test_dot_prefix(self):
        assert q('.server.host', SERVER_KVL) == 'localhost'


# ---------------------------------------------------------------------------
# Array operations
# ---------------------------------------------------------------------------

TAGS_KVL = "tags = web\ntags = api\ntags = production\n"

class TestArrayOps:
    def test_iter(self):
        assert q('tags[]', TAGS_KVL) == ['web', 'api', 'production']

    def test_index_first(self):
        assert q('tags[0]', TAGS_KVL) == 'web'

    def test_index_last(self):
        assert q('tags[2]', TAGS_KVL) == 'production'

    def test_negative_index(self):
        assert q('tags[-1]', TAGS_KVL) == 'production'
        assert q('tags[-2]', TAGS_KVL) == 'api'

    def test_slice(self):
        assert q('tags[1:3]', TAGS_KVL) == ['api', 'production']

    def test_slice_from_start(self):
        assert q('tags[:2]', TAGS_KVL) == ['web', 'api']

    def test_slice_to_end(self):
        assert q('tags[1:]', TAGS_KVL) == ['api', 'production']

    def test_slice_empty(self):
        assert q('tags[5:10]', TAGS_KVL) == []

    def test_slice_single_element_is_list(self):
        result = q('tags[0:1]', TAGS_KVL)
        assert result == ['web']

    def test_out_of_bounds_raises(self):
        with pytest.raises(KvqIndexError):
            q('tags[10]', TAGS_KVL)

    def test_negative_slice(self):
        assert q('tags[-2:]', TAGS_KVL) == ['api', 'production']

    def test_iter_on_string_raises(self):
        with pytest.raises(KvqTypeError):
            q('server.host[]', SERVER_KVL)


# ---------------------------------------------------------------------------
# Pipe functions
# ---------------------------------------------------------------------------

SCORES_KVL = "scores = 95\nscores = 87\nscores = 92\n"

class TestPipeFunctions:
    def test_length_string(self):
        # Critical regression: D returned 1 for this
        assert q('server.host | length', SERVER_KVL) == 9  # len('localhost')

    def test_length_list(self):
        # Critical regression: D returned 1 for this
        assert q('tags | length', TAGS_KVL) == 3

    def test_length_dict(self):
        # Critical regression: D returned 1 for this
        assert q('server | length', SERVER_KVL) == 3  # host, port, ssl

    def test_length_after_iter(self):
        assert q('scores[] | length', SCORES_KVL) == 3

    def test_max_numeric(self):
        assert q('scores | max', SCORES_KVL) == 95

    def test_min_numeric(self):
        assert q('scores | min', SCORES_KVL) == 87

    def test_sum_numeric(self):
        assert q('scores | sum', SCORES_KVL) == 274

    def test_max_string(self):
        data = {'tags': ['web', 'api', 'production']}
        assert execute('tags | max', data) == max(['web', 'api', 'production'])

    def test_keys_on_root(self):
        data = kvl.loads('name = test\nport = 8080\n')
        result = execute('. | keys', data)
        assert set(result) == {'name', 'port'}

    def test_keys_on_nested(self):
        result = q('server | keys', SERVER_KVL)
        assert set(result) == {'host', 'port', 'ssl'}

    def test_sum_non_numeric_raises(self):
        with pytest.raises(KvqTypeError):
            q('tags | sum', TAGS_KVL)

    def test_keys_on_non_object_raises(self):
        with pytest.raises(KvqTypeError):
            q('tags | keys', TAGS_KVL)

    def test_min_empty_raises(self):
        with pytest.raises(KvqError):
            execute('items | min', {'items': []})


# ---------------------------------------------------------------------------
# Nested iteration
# ---------------------------------------------------------------------------

API_KVL = """\
api =
  v1 =
    endpoints =
      = users
      = posts
    rate_limit = 100
  v2 =
    endpoints =
      = users
      = posts
      = files
    rate_limit = 200
"""

SERVERS_KVL = "servers =\n  web1 =\n    port = 80\n  web2 =\n    port = 8080\n"

class TestNestedIteration:
    def test_api_keys(self):
        result = q('api | keys', API_KVL)
        assert set(result) == {'v1', 'v2'}

    def test_api_iter_rate_limit_max(self):
        assert q('api[].rate_limit | max', API_KVL) == 200

    def test_servers_iter_gives_objects(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[]', data)
        assert isinstance(result, list)
        assert len(result) == 2
        assert result[0] == {'port': '80'}
        assert result[1] == {'port': '8080'}

    def test_servers_iter_port(self):
        data = kvl.loads(SERVERS_KVL)
        result = execute('servers[].port', data)
        assert result == ['80', '8080']

    def test_list_of_dicts(self):
        data = {
            'servers': [
                {'name': 'web1', 'port': '80'},
                {'name': 'web2', 'port': '8080'},
            ]
        }
        assert execute('servers[].name', data) == ['web1', 'web2']
        assert execute('servers[0].port', data) == '80'


# ---------------------------------------------------------------------------
# Edge cases: unicode, escaped keys, multiline, empty
# ---------------------------------------------------------------------------

class TestEdgeCases:
    def test_unicode_values(self):
        text = open(os.path.join(FIXTURES, 'edge/unicode.kvl')).read()
        assert q('name', text) == 'élève'
        assert q('city', text) == '東京'
        assert q('greeting', text) == '👋 hello'

    def test_hyphenated_key(self):
        assert q('rate-limit', 'rate-limit = 100\n') == '100'

    def test_equals_in_key(self):
        text = open(os.path.join(FIXTURES, 'escape/separator-in-key.kvl')).read()
        assert q('x=y', text) == 'equation'
        assert q('a=b=c', text) == 'multi'

    def test_deeply_nested(self):
        text = open(os.path.join(FIXTURES, 'edge/deeply-nested.kvl')).read()
        assert q('a.b.c.d.e', text) == 'value'

    def test_multiline_value_normalised(self):
        text = open(os.path.join(FIXTURES, 'edge/multiline.kvl')).read()
        result = q('description', text)
        # compact() strips leading newline/indent
        assert 'This is a long' in result
        assert not result.startswith('\n')
        assert not result.startswith('    ')

    def test_empty_section(self):
        text = open(os.path.join(FIXTURES, 'edge/empty-values.kvl')).read()
        result = q('section', text)
        assert result == {}

    def test_empty_iter(self):
        data = {'items': {}}
        assert execute('items[]', data) == []

    def test_missing_top_level_raises(self):
        with pytest.raises(KvqPathError):
            execute('missing', {'name': 'test'})

    def test_underscored_key(self):
        assert q('max_retries', 'max_retries = 5\n') == '5'

    def test_numeric_string_preserved(self):
        assert q('port', 'port = 8080\n') == '8080'

    def test_boolean_string_preserved(self):
        assert q('ssl', 'ssl = true\n') == 'true'

    def test_length_utf8_aware(self):
        # '东京' has 2 characters; len() gives 2 in Python 3
        assert q('city | length', 'city = 东京\n') == 2


# ---------------------------------------------------------------------------
# Multi-file merge
# ---------------------------------------------------------------------------

class TestMultiFileMerge:
    def test_new_key_from_extra(self):
        result = query('host = localhost\n', 'port', extra_texts=['port = 5432\n'])
        assert result == '5432'

    def test_scalar_override(self):
        result = query('debug = false\n', 'debug', extra_texts=['debug = true\n'])
        assert result == 'true'

    def test_unchanged_key(self):
        result = query('app =\n  name = MyApp\n  debug = false\n', 'app.name',
                       extra_texts=['app =\n  debug = true\n'])
        assert result == 'MyApp'

    def test_nested_new_key(self):
        result = query('app =\n  name = MyApp\n', 'app.port',
                       extra_texts=['app =\n  port = 3000\n'])
        assert result == '3000'


# ---------------------------------------------------------------------------
# Conformance against all fixtures
# ---------------------------------------------------------------------------

class TestConformanceFixtures:
    """Smoke-test: load each fixture, run a root query, confirm no crash."""

    FIXTURE_DIRS = ['core', 'valid', 'edge']

    def _fixture_files(self):
        paths = []
        for d in self.FIXTURE_DIRS:
            dpath = os.path.join(FIXTURES, d)
            if os.path.isdir(dpath):
                for fname in os.listdir(dpath):
                    if fname.endswith('.kvl'):
                        paths.append(os.path.join(dpath, fname))
        return paths

    def test_root_query_all_fixtures(self):
        for path in self._fixture_files():
            text = open(path).read()
            try:
                data = kvl.loads(text)
                result = execute('.', data)
                assert result is not None or result == {}
            except Exception as e:
                pytest.fail(f"fixture {os.path.basename(path)}: {e}")

    def test_keys_query_all_fixtures(self):
        for path in self._fixture_files():
            text = open(path).read()
            try:
                data = kvl.loads(text)
                if not data:
                    continue
                result = execute('. | keys', data)
                assert isinstance(result, list)
            except Exception as e:
                pytest.fail(f"fixture {os.path.basename(path)}: {e}")

    def test_escape_separator_in_key(self):
        text = open(os.path.join(FIXTURES, 'escape/separator-in-key.kvl')).read()
        data = kvl.loads(text)
        for key in data:
            result = execute(key, data)
            assert result is not None

    def test_escape_separator_in_value(self):
        text = open(os.path.join(FIXTURES, 'escape/separator-in-value.kvl')).read()
        assert q('equation', text) == 'x=y+z'
        assert q('url', text).startswith('https://')
