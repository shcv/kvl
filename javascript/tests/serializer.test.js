import { describe, it, expect } from 'vitest';
import { dumps, loads, parse, compact, KvlConfig } from '../src/index.js';

describe('Basic serialization', () => {
  it('roundtrips simple data', () => {
    const data = { name: 'test', version: '1.0' };
    const serialized = dumps(data);
    expect(loads(serialized)).toEqual(data);
  });

  it('roundtrips nested data', () => {
    const data = { server: { host: 'localhost', port: '8080' } };
    const serialized = dumps(data);
    expect(loads(serialized)).toEqual(data);
  });

  it('roundtrips list data (categorical format)', () => {
    const data = { tags: ['web', 'api', 'production'] };
    const serialized = dumps(data);
    expect(loads(serialized)).toEqual(data);
  });

  it('handles empty data', () => {
    expect(dumps({})).toBe('');
    expect(dumps(null)).toBe('');
  });
});

describe('Header inclusion', () => {
  it('includes header when requested', () => {
    const data = { key: 'value' };
    const result = dumps(data, undefined, { includeHeader: true });
    expect(result).toMatch(/^#= kvl 1\.0/);
    expect(loads(result)).toEqual(data);
  });

  it('includes header with empty data', () => {
    const result = dumps({}, undefined, { includeHeader: true });
    expect(result).toMatch(/^#= kvl 1\.0/);
  });
});

describe('Complex structures', () => {
  it('roundtrips deeply nested structures', () => {
    const data = { level1: { level2: { level3: { value: 'deep' } } } };
    const serialized = dumps(data);
    expect(loads(serialized)).toEqual(data);
  });

  it('roundtrips mixed lists and objects', () => {
    const data = {
      servers: {
        web: { hosts: ['web1', 'web2'], port: '80' },
        api: { hosts: ['api1', 'api2'], port: '8080' },
      },
    };
    const serialized = dumps(data);
    expect(loads(serialized)).toEqual(data);
  });

  it('roundtrips empty nested structures', () => {
    const data = { empty_dict: {}, dict_with_empty: { nested_empty: {} } };
    const serialized = dumps(data);
    const reparsed = loads(serialized);
    expect(reparsed).toEqual({ empty_dict: {}, dict_with_empty: 'nested_empty' });
  });
});

describe('Formatting options', () => {
  it('roundtrips with different separators', () => {
    const data = { host: 'localhost', port: '8080' };

    const colonConfig = new KvlConfig({ separator: ':' });
    const colonResult = dumps(data, colonConfig, { includeHeader: true });
    expect(colonResult).toContain('#: kvl 1.0');
    expect(loads(colonResult)).toEqual(data);

    const arrowConfig = new KvlConfig({ separator: '->' });
    const arrowResult = dumps(data, arrowConfig, { includeHeader: true });
    expect(arrowResult).toContain('#-> kvl 1.0');
    expect(loads(arrowResult)).toEqual(data);
  });
});

describe('List marker serialization', () => {
  it('auto-detects list markers from config', () => {
    const data = { items: ['first', 'second', 'third'] };
    const config = new KvlConfig({ listMarkers: '-' });
    const result = dumps(data, config, { includeHeader: true });
    expect(result).toContain('#= kvl 1.0 -');
    expect(result).toContain('- first');
    expect(result).toContain('- second');
    expect(result).toContain('- third');
  });

  it('supports explicit list marker override', () => {
    const data = { tasks: ['design', 'implement', 'test'] };
    const config = new KvlConfig({ listMarkers: '-' });
    const result = dumps(data, config, { includeHeader: true, listMarker: '+' });
    expect(result).toContain('#= kvl 1.0 -');
    expect(result).toContain('+ design');
  });

  it('forces categorical format with empty list marker', () => {
    const data = { items: ['first', 'second'] };
    const config = new KvlConfig({ listMarkers: '-' });
    const result = dumps(data, config, { listMarker: '' });
    expect(result).toContain('first =');
    expect(result).toContain('second =');
    expect(result).not.toContain('- first');
  });

  it('uses categorical format when no list markers configured', () => {
    const data = { items: ['first', 'second'] };
    const result = dumps(data);
    expect(result).toContain('first =');
    expect(result).toContain('second =');
    expect(result).not.toContain('- first');
  });

  it('roundtrips with list markers', () => {
    const data = { features: ['auth', 'api', 'logs'] };
    const config = new KvlConfig({ listMarkers: '-' });
    const serialized = dumps(data, config, { includeHeader: true });
    expect(loads(serialized)).toEqual(data);
  });

  it('handles complex nested structures with list markers', () => {
    const data = {
      servers: [
        { name: 'web1', port: '80' },
        { name: 'web2', port: '8080' },
      ],
    };
    const config = new KvlConfig({ listMarkers: '-' });
    const result = dumps(data, config);
    expect(result).toContain('- name = web1');
    expect(result).toContain('- name = web2');
  });

  it('handles mixed data types with list markers', () => {
    const data = {
      simple_list: ['item1', 'item2'],
      simple_key: 'value',
      nested_object: { inner: 'data' },
    };
    const config = new KvlConfig({ listMarkers: '-' });
    const result = dumps(data, config, { includeHeader: true });
    expect(result).toContain('- item1');
    expect(result).toContain('- item2');
    expect(result).toContain('simple_key = value');
    expect(result).toContain('nested_object =');
    expect(result).toContain('inner = data');
  });
});
