"""KVQ Hybrid query engine: D's tokenizer + C's execution model.

Design:
  - Parse: D-style regex tokenizer with position info in errors
  - Compile: flat op list (same as both C and D)
  - Execute: C-style — compact() data upfront, operate on clean Python types
  - Public API: D-style query(text, q, extra_texts=[...]) returning List[str]

Op types:
  ('dot',)         - identity / root (explicit unlike C)
  ('get', key)     - map key access over stream
  ('index', n)     - nth element (negative ok)
  ('slice', lo, hi)- key/element range (None = open end)
  ('iter',)        - flatmap: fan out each item into its elements
  ('apply', fn)    - terminal aggregator: length|keys|min|max|sum

Key improvements over C:
  - Tokenizer-based parser: position info in errors, handles hyphenated keys
  - Identifier regex extended to accept = and \\ in key names
  - Explicit ('dot',) op in op list
  - _resolve_slice() for explicit negative-bound normalization
  - extra_texts parameter for multi-file merge

Key improvements over D:
  - Correct length: len(str), len(list), len(dict), not count of stream items
  - Correct keys(): returns list of strings, not newline-joined string
  - No categorical wrapping in results
  - = and \\ accepted in key names
  - Multiline values normalised by compact()
"""

from __future__ import annotations

import re
from typing import Any, Generator, Iterator, List, Optional, Tuple

import kvl
from kvl.transform import merge as kvl_merge


# ---------------------------------------------------------------------------
# Error types
# ---------------------------------------------------------------------------

class KvqError(Exception):
    """Base error for kvq query execution."""


class KvqParseError(KvqError):
    """Invalid query syntax."""
    def __init__(self, msg: str, pos: int = -1):
        self.pos = pos
        super().__init__(msg + (f" (at position {pos})" if pos >= 0 else ""))


class KvqPathError(KvqError):
    """Key not found or invalid path."""


class KvqTypeError(KvqError):
    """Operation not supported on the given type."""


class KvqIndexError(KvqError):
    """Array index out of bounds."""


# ---------------------------------------------------------------------------
# Tokeniser (D-style, extended identifier charset)
# ---------------------------------------------------------------------------

# Identifiers: start with non-special char, continue with non-special chars.
# "Special" chars are: whitespace, . [ ] : | (the grammar punctuation).
# This means = and \ are valid in identifiers, matching what KVL keys allow.
_IDENT_RE = re.compile(r'[^\s.\[\]:|]+')

_TOK = re.compile(
    r'(?P<string>"(?:[^"\\]|\\.)*")'   # double-quoted string (must come before ident/cmpop)
    r'|(?P<cmpop>>=|<=|==|!=|>|<)'     # comparison operators (multi-char first, before ident)
    r'|(?P<ident>[^\s.\[\]:()|<>!"]+)' # identifier: excludes grammar/operator/quote chars
    r'|(?P<dot>\.)'
    r'|(?P<lbr>\[)'
    r'|(?P<rbr>\])'
    r'|(?P<lparen>\()'
    r'|(?P<rparen>\))'
    r'|(?P<colon>:)'
    r'|(?P<pipe>\|)'
    r'|(?P<ws>\s+)'
)

# Note: 'int' must be checked before 'ident' would otherwise swallow it.
# But since ident matches any non-special char including digits, we need
# the int pattern to come first when we need to distinguish them inside [].
# Strategy: use ident everywhere in paths, only look for int inside [].

Op = Tuple  # ('dot',) | ('get', key) | ('index', n) | ('slice', lo, hi) | ('iter',) | ('apply', fn) | ('select', predicate)

_FUNCTIONS = frozenset({'length', 'keys', 'min', 'max', 'sum'})

# select() comparison operators
_SELECT_OPS = frozenset({'==', '!=', '>', '<', '>=', '<='})

# Predicate: (path_parts, operator, value)
# path_parts is a list of strings (e.g. ['field'] or ['config', 'ssl'])
# '.' alone means identity (the item itself)
Predicate = Tuple  # (path_parts: List[str], op: str, value: Any)


def _tokenise(query: str) -> List[Tuple[str, str, int]]:
    """Return list of (kind, value, pos), skipping whitespace."""
    tokens = []
    pos = 0
    while pos < len(query):
        m = _TOK.match(query, pos)
        if m is None:
            raise KvqParseError(f"Unexpected character {query[pos]!r}", pos)
        if m.lastgroup != 'ws':
            # Re-classify: integers inside brackets will be re-checked during
            # bracket parsing. At tokenise time, just distinguish ident vs punct.
            tokens.append((m.lastgroup, m.group(), m.start()))
        pos = m.end()
    return tokens


def parse_query(query: str) -> List[Op]:
    """Compile query string to flat list of ops.

    Grammar:
      query       := path_expr ('|' fn_name)*
      path_expr   := ('.')? path_component ('.' path_component)*
      path_component := identifier? ('[' bracket_op ']')*
      bracket_op  := ε | integer | integer? ':' integer?
    """
    query = query.strip()
    if not query:
        raise KvqParseError("Empty query")

    tokens = _tokenise(query)
    ops: List[Op] = []
    i = 0
    n = len(tokens)

    def peek(offset: int = 0) -> Optional[Tuple[str, str, int]]:
        idx = i + offset
        return tokens[idx] if idx < n else None

    def consume() -> Tuple[str, str, int]:
        nonlocal i
        tok = tokens[i]
        i += 1
        return tok

    def expect(kind: str) -> Tuple[str, str, int]:
        tok = peek()
        if tok is None or tok[0] != kind:
            got = repr(tok[1]) if tok else 'end of query'
            pos = tok[2] if tok else len(query)
            raise KvqParseError(f"Expected {kind!r}, got {got}", pos)
        return consume()

    # -- Parse path expression --

    if not tokens:
        raise KvqParseError("Empty query")

    first = peek()
    if first[0] == 'dot':
        ops.append(('dot',))
        consume()
        # After '.', if immediately followed by ident (e.g. .server.host),
        # consume that ident as a get op and fall through to the loop.
        if i < n and peek()[0] == 'ident':
            ops.append(('get', peek()[1]))
            consume()
    elif first[0] == 'ident':
        ops.append(('get', first[1]))
        consume()
    elif first[0] == 'lbr':
        # [n] at root level: index/slice/iter on root
        pass
    else:
        raise KvqParseError(
            f"Query must start with '.', identifier, or '[', got {first[1]!r}",
            first[2]
        )

    # Continue path: .key, [bracket_op]
    while i < n:
        tok = peek()
        if tok[0] == 'pipe':
            break
        if tok[0] == 'dot':
            consume()
            next_tok = peek()
            if next_tok is None or next_tok[0] != 'ident':
                pos = next_tok[2] if next_tok else len(query)
                raise KvqParseError("Expected identifier after '.'", pos)
            ops.append(('get', next_tok[1]))
            consume()
        elif tok[0] == 'lbr':
            consume()  # consume '['
            inner = peek()
            if inner is None:
                raise KvqParseError("Unclosed '['", tok[2])
            if inner[0] == 'rbr':
                # [] iterator
                ops.append(('iter',))
                consume()
            elif inner[0] == 'colon':
                # [:hi]
                consume()
                hi_tok = peek()
                if hi_tok and hi_tok[0] == 'int':
                    hi = int(hi_tok[1])
                    consume()
                elif hi_tok and hi_tok[0] == 'ident' and _is_integer(hi_tok[1]):
                    hi = int(hi_tok[1])
                    consume()
                else:
                    hi = None
                expect('rbr')
                ops.append(('slice', None, hi))
            elif inner[0] in ('int', 'ident') and _is_integer(inner[1]):
                lo = int(inner[1])
                consume()
                after = peek()
                if after and after[0] == 'colon':
                    consume()
                    hi_tok = peek()
                    if hi_tok and hi_tok[0] in ('int', 'ident') and _is_integer(hi_tok[1]):
                        hi = int(hi_tok[1])
                        consume()
                    else:
                        hi = None
                    expect('rbr')
                    ops.append(('slice', lo, hi))
                else:
                    expect('rbr')
                    ops.append(('index', lo))
            else:
                raise KvqParseError(
                    f"Unexpected token inside '[': {inner[1]!r}", inner[2]
                )
        else:
            raise KvqParseError(
                f"Unexpected token in path: {tok[1]!r}", tok[2]
            )

    # -- Parse pipe functions --
    while i < n:
        tok = peek()
        if tok[0] != 'pipe':
            raise KvqParseError(f"Expected '|', got {tok[1]!r}", tok[2])
        consume()
        fn_tok = peek()
        if fn_tok is None or fn_tok[0] != 'ident':
            pos = fn_tok[2] if fn_tok else len(query)
            raise KvqParseError("Expected function name after '|'", pos)
        if fn_tok[1] == 'select':
            consume()  # consume 'select'
            select_op, i = _parse_select_predicate(tokens, i, n, query)
            ops.append(select_op)
        elif fn_tok[1] not in _FUNCTIONS:
            raise KvqParseError(
                f"Unknown function {fn_tok[1]!r} (known: {', '.join(sorted(_FUNCTIONS))})",
                fn_tok[2]
            )
        else:
            ops.append(('apply', fn_tok[1]))
            consume()

    return ops


def _is_integer(s: str) -> bool:
    """Return True if s represents an integer (possibly negative)."""
    try:
        int(s)
        return True
    except (ValueError, TypeError):
        return False


def _parse_select_predicate(
    tokens: List[Tuple[str, str, int]],
    i: int,
    n: int,
    query: str,
) -> Tuple[Op, int]:
    """Parse select(predicate) starting at i (after 'select' token consumed).

    Predicate grammar:
      select '(' path cmpop value ')'
      path   := '.' | '.' ident ('.' ident)*
      cmpop  := '==' | '!=' | '>' | '<' | '>=' | '<='
      value  := '"' chars '"' | ident | int | 'true' | 'false'

    Returns (('select', predicate), new_i).
    """
    def _tok(offset: int = 0):
        idx = i + offset
        return tokens[idx] if idx < n else None

    if _tok() is None or _tok()[0] != 'lparen':
        pos = _tok()[2] if _tok() else len(query)
        raise KvqParseError("Expected '(' after 'select'", pos)
    i += 1  # consume '('

    # Parse path: starts with '.'
    if _tok() is None or _tok()[0] != 'dot':
        pos = _tok()[2] if _tok() else len(query)
        raise KvqParseError("select predicate must start with '.'", pos)
    i += 1  # consume '.'

    path_parts: List[str] = []
    # If immediately followed by ident, collect path parts
    while _tok() is not None and _tok()[0] == 'ident':
        path_parts.append(_tok()[1])
        i += 1
        if _tok() is not None and _tok()[0] == 'dot':
            i += 1  # consume '.' between path parts
        else:
            break

    # Parse operator
    if _tok() is None or _tok()[0] != 'cmpop':
        pos = _tok()[2] if _tok() else len(query)
        raise KvqParseError("Expected comparison operator in select predicate", pos)
    op = _tok()[1]
    i += 1

    # Parse value
    if _tok() is None:
        raise KvqParseError("Expected value in select predicate", len(query))
    val_tok = _tok()
    if val_tok[0] == 'string':
        # Decode double-quoted string: strip quotes, unescape \" and \\
        raw = val_tok[1][1:-1]  # strip surrounding "
        value: Any = raw.replace('\\"', '"').replace('\\\\', '\\')
        i += 1
    elif val_tok[0] in ('ident', 'int'):
        raw = val_tok[1]
        # Coerce booleans
        if raw == 'true':
            value = True
        elif raw == 'false':
            value = False
        else:
            value = raw
        i += 1
    else:
        raise KvqParseError(
            f"Unexpected token in select value: {val_tok[1]!r}", val_tok[2]
        )

    # Expect closing ')'
    if _tok() is None or _tok()[0] != 'rparen':
        pos = _tok()[2] if _tok() else len(query)
        raise KvqParseError("Expected ')' to close select(...)", pos)
    i += 1  # consume ')'

    predicate = (path_parts, op, value)
    return ('select', predicate), i


# ---------------------------------------------------------------------------
# Slice helper (D-style explicit normalisation)
# ---------------------------------------------------------------------------

def _resolve_slice(lo: Optional[int], hi: Optional[int], length: int) -> Tuple[int, int]:
    """Convert optional slice bounds to concrete indices (Python semantics)."""
    lo = 0 if lo is None else lo
    hi = length if hi is None else hi
    if lo < 0:
        lo = max(0, length + lo)
    if hi < 0:
        hi = max(0, length + hi)
    lo = min(lo, length)
    hi = min(hi, length)
    return lo, hi


# ---------------------------------------------------------------------------
# Execution engine (C-style: clean Python types from compact())
# ---------------------------------------------------------------------------

def _coerce_to_list(item: Any, op: str) -> List[Any]:
    """Coerce a value to a positional list for index/slice/iter operations."""
    if isinstance(item, list):
        return item
    if isinstance(item, dict):
        # Categorical dict: all-empty-values → list of keys (strings)
        #                   with-values → list of values (objects)
        if not item:
            return []
        if all(v == {} for v in item.values()):
            return list(item.keys())
        return list(item.values())
    raise KvqTypeError(f"{op} not supported on {type(item).__name__}")


def _run_get(key: str, stream: Iterator[Any]) -> Generator:
    for item in stream:
        if isinstance(item, dict):
            if key not in item:
                raise KvqPathError(f"Key {key!r} not found")
            yield item[key]
        else:
            raise KvqTypeError(
                f"Cannot access key {key!r} on {type(item).__name__}"
            )


def _run_iter(stream: Iterator[Any]) -> Generator:
    for item in stream:
        lst = _coerce_to_list(item, '[]')
        yield from lst


def _run_index(idx: int, stream: Iterator[Any]) -> Generator:
    for item in stream:
        lst = _coerce_to_list(item, f'[{idx}]')
        lo, hi = _resolve_slice(idx, idx + 1 if idx >= 0 else idx + len(lst) + 1, len(lst))
        # Simpler: use Python indexing directly
        try:
            yield lst[idx]
        except IndexError:
            raise KvqIndexError(
                f"Index {idx} out of range for sequence of length {len(lst)}"
            )


def _run_slice(lo: Optional[int], hi: Optional[int], stream: Iterator[Any]) -> Generator:
    for item in stream:
        lst = _coerce_to_list(item, '[:]')
        a, b = _resolve_slice(lo, hi, len(lst))
        yield lst[a:b]


def _coerce_for_compare(a: Any, b: Any) -> Tuple[Any, Any]:
    """Coerce a and b for comparison.

    Rules (in order):
    1. If either side is bool, convert both to lowercase strings ('true'/'false')
       so KVL string values like 'true' compare correctly.
    2. If both sides parse as numbers, compare numerically.
    3. Otherwise compare as strings.
    """
    if isinstance(a, bool) or isinstance(b, bool):
        def _bool_str(v: Any) -> str:
            if isinstance(v, bool):
                return 'true' if v else 'false'
            return str(v)
        return _bool_str(a), _bool_str(b)
    try:
        na = float(a) if '.' in str(a) else int(a)
        nb = float(b) if '.' in str(b) else int(b)
        return na, nb
    except (ValueError, TypeError):
        return str(a), str(b)


def _eval_predicate(item: Any, predicate: Predicate) -> bool:
    """Evaluate a select predicate against a single item."""
    path_parts, op, value = predicate

    # Resolve path
    if not path_parts:
        # '.' alone: item itself
        lhs = item
    else:
        lhs = item
        for part in path_parts:
            if not isinstance(lhs, dict):
                raise KvqTypeError(
                    f"Cannot access key {part!r} on {type(lhs).__name__}"
                )
            if part not in lhs:
                raise KvqPathError(f"Key {part!r} not found in select predicate")
            lhs = lhs[part]

    # Coerce and compare
    lhs_c, rhs_c = _coerce_for_compare(lhs, value)
    try:
        if op == '==':
            return lhs_c == rhs_c
        if op == '!=':
            return lhs_c != rhs_c
        if op == '>':
            return lhs_c > rhs_c
        if op == '<':
            return lhs_c < rhs_c
        if op == '>=':
            return lhs_c >= rhs_c
        if op == '<=':
            return lhs_c <= rhs_c
    except TypeError:
        # Incomparable types: treat as not equal
        return op == '!='
    raise KvqError(f"Unknown select operator: {op!r}")


def _run_select(predicate: Predicate, stream: Iterator[Any]) -> Generator:
    for item in stream:
        if _eval_predicate(item, predicate):
            yield item


def _run_apply(fn: str, stream: Iterator[Any]) -> Any:
    """Consume stream and apply aggregation. Returns a single Python value."""
    items = list(stream)

    if fn == 'length':
        # If single value: len of that value. If multiple: count of items.
        if len(items) == 1:
            val = items[0]
            if isinstance(val, str):
                return len(val)
            if isinstance(val, (list, dict)):
                return len(val)
            # Single non-container: type error per spec
            raise KvqTypeError(f"length not supported on {type(val).__name__}")
        return len(items)

    if fn == 'keys':
        if len(items) == 1 and isinstance(items[0], dict):
            return list(items[0].keys())
        raise KvqTypeError("keys requires a single object")

    if fn in ('min', 'max', 'sum'):
        # Flatten one level (slices yield sub-lists)
        flat: List[Any] = []
        for it in items:
            if isinstance(it, list):
                flat.extend(it)
            else:
                flat.append(it)

        if not flat:
            raise KvqError(f"{fn} requires non-empty input")

        # Numeric conversion
        nums = []
        for v in flat:
            try:
                nums.append(float(v) if '.' in str(v) else int(v))
            except (ValueError, TypeError):
                nums.append(v)

        if fn == 'sum':
            total = 0
            for num in nums:
                if not isinstance(num, (int, float)):
                    raise KvqTypeError(f"sum requires numeric values, got {num!r}")
                total += num
            return total
        if fn == 'min':
            try:
                return min(nums)
            except TypeError:
                return min(str(x) for x in nums)
        if fn == 'max':
            try:
                return max(nums)
            except TypeError:
                return max(str(x) for x in nums)

    raise KvqError(f"Unknown function: {fn!r}")


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def execute(query: str, data: Any) -> Any:
    """Execute a KVQ query against compacted KVL data.

    Args:
        query: KVQ query string.
        data:  Compacted KVL data (output of kvl.loads()).

    Returns:
        Query result: scalar, list, or dict.

    Raises:
        KvqParseError, KvqPathError, KvqTypeError, KvqIndexError, KvqError
    """
    ops = parse_query(query)
    stream: Iterator[Any] = iter([data])

    for op in ops:
        kind = op[0]
        if kind == 'dot':
            pass  # identity: stream unchanged
        elif kind == 'get':
            stream = _run_get(op[1], stream)
        elif kind == 'iter':
            stream = _run_iter(stream)
        elif kind == 'index':
            stream = _run_index(op[1], stream)
        elif kind == 'slice':
            stream = _run_slice(op[1], op[2], stream)
        elif kind == 'apply':
            return _run_apply(op[1], stream)
        elif kind == 'select':
            stream = _run_select(op[1], stream)
        else:
            raise KvqError(f"Unknown op: {kind!r}")

    result = list(stream)
    if len(result) == 1:
        return result[0]
    return result


def query(kvl_text: str, query_str: str, extra_texts: Optional[List[str]] = None) -> Any:
    """Parse KVL text(s), compact, execute query, return result.

    Args:
        kvl_text:    Primary KVL document text.
        query_str:   KVQ query string.
        extra_texts: Additional KVL texts merged in order (later overrides earlier
                     for disjoint keys; conflicting scalar keys become categorical).

    Returns:
        Query result: scalar, list, or dict.
    """
    data = kvl.loads(kvl_text)
    if extra_texts:
        for text in extra_texts:
            extra = kvl.loads(text)
            # merge on compacted dicts: last-wins for simple string values
            data = _merge_compacted(data, extra)
    return execute(query_str, data)


def _merge_compacted(base: Any, overlay: Any) -> Any:
    """Merge two compacted dicts (overlay wins for scalar conflicts)."""
    if isinstance(base, dict) and isinstance(overlay, dict):
        result = base.copy()
        for k, v in overlay.items():
            result[k] = _merge_compacted(result[k], v) if k in result else v
        return result
    # Scalar or type mismatch: overlay wins
    return overlay
