import { describe, it, expect } from 'vitest';
import { loads, parse, keyvals, KvlConfig, KvlParseError } from '../src/index.js';

describe('Basic parsing', () => {
  it('parses simple key-value pairs', () => {
    expect(loads('key = value')).toEqual({ key: 'value' });
  });

  it('parses multiple keys at the same level', () => {
    const content = `
        name = test
        version = 1.0
        active = true
    `;
    expect(loads(content)).toEqual({ name: 'test', version: '1.0', active: 'true' });
  });

  it('creates list structure for repeated keys (compacted)', () => {
    const content = `
        tags = web
        tags = api
        tags = production
    `;
    expect(loads(content)).toEqual({ tags: ['web', 'api', 'production'] });
  });

  it('handles empty values', () => {
    const content = `
        config =
        data =
    `;
    expect(loads(content)).toEqual(['config', 'data']);
  });

  it('parses with different separators', () => {
    expect(loads('#: kvl 1.0\nkey: value')).toEqual({ key: 'value' });
    expect(loads('#-> kvl 1.0\nkey -> value')).toEqual({ key: 'value' });
    expect(loads('#:= kvl 1.0\nkey := value')).toEqual({ key: 'value' });
  });
});

describe('Multiline values', () => {
  it('collects indented continuation lines', () => {
    const content = `a = a\n b\n c\n  d\n`;
    // loads() dedents continuation lines by min indent (1)
    expect(loads(content)).toEqual({ a: 'a\nb\nc\n d' });
  });

  it('handles empty initial value with indented lines', () => {
    const content = `text =\n This is line 1\n This is line 2\n  This is indented more\n`;
    // loads() strips leading \n and dedents by min indent (1)
    expect(loads(content)).toEqual({
      text: 'This is line 1\nThis is line 2\n This is indented more',
    });
  });

  it('preserves blank lines in multiline values', () => {
    const content = `poem =\n Roses are red\n\n Violets are blue\n`;
    // loads() strips leading \n and dedents by min indent (1)
    expect(loads(content)).toEqual({
      poem: 'Roses are red\n\nViolets are blue',
    });
  });

  it('continuation lines do not become new keys', () => {
    const content = `config = start\n continuation1\n continuation2\nnext_key = value\n`;
    // loads() dedents continuation lines by min indent (1)
    expect(loads(content)).toEqual({
      config: 'start\ncontinuation1\ncontinuation2',
      next_key: 'value',
    });
  });

  it('handles empty value with indented lines without separator', () => {
    const content = `a =\n  b\n  c\n`;
    // loads() strips leading \n and dedents by min indent (2)
    expect(loads(content)).toEqual({ a: 'b\nc' });
  });
});

describe('Nested structures', () => {
  it('parses simple nesting', () => {
    const content = `
        server =
          host = localhost
          port = 8080
    `;
    expect(loads(content)).toEqual({ server: { host: 'localhost', port: '8080' } });
  });

  it('parses multiple levels of nesting', () => {
    const content = `
        database =
          primary =
            host = db1.example.com
            port = 5432
          secondary =
            host = db2.example.com
            port = 5432
    `;
    expect(loads(content)).toEqual({
      database: {
        primary: { host: 'db1.example.com', port: '5432' },
        secondary: { host: 'db2.example.com', port: '5432' },
      },
    });
  });

  it('handles repeated keys within nested structures', () => {
    const content = `
        servers =
          web =
            port = 80
            port = 443
          api =
            port = 8080
            port = 8081
    `;
    expect(loads(content)).toEqual({
      servers: {
        web: { port: ['80', '443'] },
        api: { port: ['8080', '8081'] },
      },
    });
  });

  it('parses very deep nesting', () => {
    const content = `
        level1 =
          level2 =
            level3 =
              level4 =
                deep_value = found
    `;
    expect(loads(content)).toEqual({
      level1: { level2: { level3: { level4: { deep_value: 'found' } } } },
    });
  });

  it('mixes flat and nested structures', () => {
    const content = `
        top_level = simple
        nested =
          inner = value
        another_top = also_simple
    `;
    expect(loads(content)).toEqual({
      top_level: 'simple',
      nested: { inner: 'value' },
      another_top: 'also_simple',
    });
  });
});

describe('Anonymous lists', () => {
  it('parses simple anonymous list with empty keys', () => {
    const content = `
        items =
          = first
          = second
          = third
    `;
    expect(loads(content)).toEqual({ items: ['first', 'second', 'third'] });
  });

  it('handles mixed regular keys with anonymous lists', () => {
    const content = `
        config =
          title = My App
          features =
            = authentication
            = api-gateway
          version = 1.0.0
    `;
    expect(loads(content)).toEqual({
      config: {
        title: 'My App',
        features: ['authentication', 'api-gateway'],
        version: '1.0.0',
      },
    });
  });
});

describe('List marker parsing', () => {
  it('parses simple list with dash marker', () => {
    const content = `#= kvl 1.0 -
        priorities =
        - high
        - medium
        - low
    `;
    expect(loads(content)).toEqual({ priorities: ['high', 'medium', 'low'] });
  });

  it('handles multiple list markers in header', () => {
    const content = `#: kvl 1.0 -+*
        priorities:
        - high
        + medium
        * low
    `;
    expect(loads(content)).toEqual({ priorities: ['high', 'medium', 'low'] });
  });

  it('requires space after list marker', () => {
    const content = `#= kvl 1.0 -
        -my-key = value
        normal-key = other
    `;
    expect(loads(content)).toEqual({ '-my-key': 'value', 'normal-key': 'other' });
  });

  it('keys can start with marker characters', () => {
    const content = `#= kvl 1.0 -+*
        -key = dash-key
        +key = plus-key
        *key = star-key
    `;
    expect(loads(content)).toEqual({
      '-key': 'dash-key',
      '+key': 'plus-key',
      '*key': 'star-key',
    });
  });

  it('parses anonymous list under a key', () => {
    const content = `#= kvl 1.0 -
        items =
          - item1
          - item2
          - item3
    `;
    expect(loads(content)).toEqual({ items: ['item1', 'item2', 'item3'] });
  });

  it('handles list items with nested key-value pairs', () => {
    const content = `#= kvl 1.0 -
        servers =
          - name = web1
          - name = web2
          - port = 80
          - port = 8080
    `;
    expect(loads(content)).toEqual({
      servers: [
        { name: 'web1' },
        { name: 'web2' },
        { port: '80' },
        { port: '8080' },
      ],
    });
  });

  it('errors on list items at top level without preceding key', () => {
    const content = `#= kvl 1.0 -
        - orphaned item
    `;
    expect(() => loads(content)).toThrow(KvlParseError);
  });

  it('errors on list items after key with value', () => {
    const content = `#= kvl 1.0 -
        key_with_value = something
        - should not attach
    `;
    expect(() => loads(content)).toThrow(KvlParseError);
  });

  it('ignores symbols without list markers in header', () => {
    const content = `#= kvl 1.0
        key = value
        - not-a-list-item = also-value
    `;
    expect(loads(content)).toEqual({ key: 'value', '- not-a-list-item': 'also-value' });
  });

  it('works with different separators', () => {
    expect(loads('#: kvl 1.0 -\n        items:\n        - first\n        - second\n'))
      .toEqual({ items: ['first', 'second'] });
    expect(loads('#-> kvl 1.0 +\n        tasks ->\n        + task1\n        + task2\n'))
      .toEqual({ tasks: ['task1', 'task2'] });
  });

  it('handles nested list structures', () => {
    const content = `#= kvl 1.0 -
        object =
          - list1 =
            - item1
            - item2
          - list2 =
            - item3
            - item4
    `;
    expect(loads(content)).toEqual({
      object: [
        { list1: ['item1', 'item2'] },
        { list2: ['item3', 'item4'] },
      ],
    });
  });

  it('parses yaml-style nested scalar lists', () => {
    const content = `#= kvl 1.0 -
        groups =
          -
            - a
            - b
          -
            - c
    `;
    expect(loads(content)).toEqual({ groups: [['a', 'b'], ['c']] });
  });

  it('treats inline nested-list sugar like the bare-marker form', () => {
    const sugar = `#= kvl 1.0 -
        groups =
          - - a
            - b
          - - c
            - d
    `;
    const bare = `#= kvl 1.0 -
        groups =
          -
            - a
            - b
          -
            - c
            - d
    `;
    expect(parse(sugar)).toEqual(parse(bare));
    expect(loads(sugar)).toEqual({ groups: [['a', 'b'], ['c', 'd']] });
  });

  it('parses yaml-style list of objects', () => {
    const content = `#= kvl 1.0 -
        servers =
          -
            name = web1
            port = 80
          -
            name = web2
            port = 81
    `;
    expect(loads(content)).toEqual({
      servers: [
        { name: 'web1', port: '80' },
        { name: 'web2', port: '81' },
      ],
    });
  });

  it('treats inline object-list sugar like the bare-marker form', () => {
    const sugar = `#= kvl 1.0 -
        servers =
          - name = web1
            port = 80
          - name = web2
            port = 81
    `;
    const bare = `#= kvl 1.0 -
        servers =
          -
            name = web1
            port = 80
          -
            name = web2
            port = 81
    `;
    expect(parse(sugar)).toEqual(parse(bare));
    expect(loads(sugar)).toEqual({
      servers: [
        { name: 'web1', port: '80' },
        { name: 'web2', port: '81' },
      ],
    });
  });

  it('handles list items with nested objects', () => {
    const content = `#= kvl 1.0 -
        servers =
          - web =
              host = web1.example.com
          - api =
              host = api1.example.com
    `;
    expect(loads(content)).toEqual({
      servers: [
        { web: { host: 'web1.example.com' } },
        { api: { host: 'api1.example.com' } },
      ],
    });
  });
});

describe('Edge cases', () => {
  it('handles empty input', () => {
    expect(loads('')).toEqual({});
    expect(loads('   ')).toEqual({});
    expect(loads('\n\n')).toEqual({});
    expect(loads('\t\t\n')).toEqual({});
  });

  it('preserves comments as keys', () => {
    const text = `/= This is a comment\nkey = value`;
    const result = loads(text);
    expect(result.key).toBe('value');
    // /= is parsed as key="/", value="This is a comment"
    expect(result['/']).toBe('This is a comment');
  });

  it('rejects line without separator', () => {
    expect(() => loads('line without separator')).toThrow(/[Mm]issing separator/);
  });

  it('handles only separator', () => {
    const result = loads('=');
    expect(result).toEqual({});
  });

  it('supports unicode', () => {
    const text = `emoji = 🚀💻\naccents = café naïve résumé\nchinese = 你好世界`;
    const result = loads(text);
    expect(result.emoji).toBe('🚀💻');
    expect(result.accents).toBe('café naïve résumé');
    expect(result.chinese).toBe('你好世界');
  });

  it('handles empty key', () => {
    const parseResult = parse(' = value');
    expect(parseResult['']).toEqual({ value: {} });
    expect(loads(' = value')).toBe('value');
  });

  it('handles special characters in values', () => {
    const text = `url = https://example.com/path\nsymbols = !@#$%^&*()_+-{}[]|\\:";'<>?,./`;
    const result = loads(text);
    expect(result.url).toBe('https://example.com/path');
  });

  it('handles separator escaping', () => {
    expect(loads('equation = x\\=y+z').equation).toBe('x=y+z');
    expect(loads('#: kvl 1.0\nurl: https\\://example.com').url).toBe('https://example.com');
    expect(loads('formula = a\\=b+c\\=d').formula).toBe('a=b+c=d');
    expect(loads('key\\=with\\=equals = value\\=with\\=equals')['key=with=equals']).toBe('value=with=equals');
  });

  it('handles double backslash escaping', () => {
    expect(loads('escaped_for_downstream = prefix\\\\=suffix').escaped_for_downstream).toBe('prefix\\=suffix');
  });

  it('handles large nested structure', () => {
    const text = `config =
    database =
        primary =
            host = db1.example.com
            port = 5432
        secondary =
            host = db2.example.com
            port = 5432
    cache =
        redis =
            host = redis.example.com
            port = 6379`;
    const result = loads(text);
    expect(result.config.database.primary.host).toBe('db1.example.com');
    expect(result.config.cache.redis.port).toBe('6379');
  });

  it('handles mixed empty and valued keys', () => {
    const text = `has_value = something\nempty_key =\nanother_value = test\nanother_empty =`;
    const parseResult = parse(text);
    expect(parseResult.empty_key).toEqual({});

    const loadsResult = loads(text);
    expect(loadsResult).toBeTypeOf('object');
    expect(loadsResult.has_value).toBe('something');
  });
});

describe('keyvals function', () => {
  it('returns list of single-key objects', () => {
    const result = keyvals('a = 1\nb = 2');
    expect(result).toEqual([{ a: '1' }, { b: '2' }]);
  });

  it('returns empty array for empty input', () => {
    expect(keyvals('')).toEqual([]);
    expect(keyvals('   ')).toEqual([]);
  });
});

describe('parse function (low-level)', () => {
  it('returns categorical model', () => {
    const result = parse('tags = web\ntags = api');
    expect(result).toEqual({ tags: { web: {}, api: {} } });
  });

  it('throws on oversized input', () => {
    const hugeInput = 'x'.repeat(11 * 1024 * 1024);
    expect(() => parse(hugeInput)).toThrow(KvlParseError);
  });
});
