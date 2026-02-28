import { describe, it, expect } from 'vitest';
import { merge, compact, expand, KvlConfig } from '../src/index.js';

describe('merge', () => {
  it('merges two disjoint dicts', () => {
    expect(merge({ a: {} }, { b: {} })).toEqual({ a: {}, b: {} });
  });

  it('recursively merges nested dicts', () => {
    const m1 = { server: { host: { localhost: {} } } };
    const m2 = { server: { port: { '8080': {} } } };
    expect(merge(m1, m2)).toEqual({
      server: { host: { localhost: {} }, port: { '8080': {} } },
    });
  });

  it('creates categorical structure from conflicting scalars', () => {
    // Both non-dict → convert to categorical and merge
    const r = merge({ a: {} }, { b: {} });
    expect(r).toEqual({ a: {}, b: {} });
  });

  it('dict wins over non-dict', () => {
    expect(merge({ a: {} }, 'string')).toEqual({ a: {} });
    expect(merge('string', { b: {} })).toEqual({ b: {} });
  });

  it('is associative: (A+B)+C = A+(B+C)', () => {
    const a = { x: { '1': {} } };
    const b = { x: { '2': {} }, y: { a: {} } };
    const c = { y: { b: {} }, z: {} };
    expect(merge(merge(a, b), c)).toEqual(merge(a, merge(b, c)));
  });

  it('handles empty dicts', () => {
    expect(merge({}, { a: {} })).toEqual({ a: {} });
    expect(merge({ a: {} }, {})).toEqual({ a: {} });
    expect(merge({}, {})).toEqual({});
  });
});

describe('compact', () => {
  it('converts singleton empty-value dict to string', () => {
    expect(compact({ value: {} })).toBe('value');
  });

  it('flattens all-empty-value dict to list', () => {
    expect(compact({ a: {}, b: {}, c: {} })).toEqual(['a', 'b', 'c']);
  });

  it('lifts singleton empty key', () => {
    expect(compact({ '': 'content' })).toBe('content');
  });

  it('recursively compacts nested structures', () => {
    const data = {
      tags: { web: {}, api: {}, prod: {} },
      name: { 'John': {} },
    };
    expect(compact(data)).toEqual({ tags: ['web', 'api', 'prod'], name: 'John' });
  });

  it('does not flatten if section header detected', () => {
    const config = new KvlConfig({ separator: '=' });
    const data = { '= Section': {}, normal: {} };
    // Section header starts with separator, so don't flatten
    expect(compact(data, config)).toEqual({ '= Section': {}, normal: {} });
  });

  it('compacts arrays recursively', () => {
    const data = [{ a: {} }, { b: {} }];
    expect(compact(data)).toEqual(['a', 'b']);
  });

  it('passes through non-dict non-array values', () => {
    expect(compact('hello')).toBe('hello');
    expect(compact(42)).toBe(42);
    expect(compact(null)).toBe(null);
  });
});

describe('expand', () => {
  it('converts list to categorical dict', () => {
    expect(expand(['a', 'b', 'c'])).toEqual({ a: {}, b: {}, c: {} });
  });

  it('recursively expands nested structures', () => {
    const data = { tags: ['web', 'api'] };
    expect(expand(data)).toEqual({ tags: { web: {}, api: {} } });
  });

  it('converts primitive to categorical', () => {
    expect(expand('hello')).toEqual({ hello: {} });
    expect(expand(42)).toEqual({ '42': {} });
  });

  it('handles null', () => {
    expect(expand(null)).toEqual({});
  });

  it('handles empty list', () => {
    expect(expand([])).toEqual({});
  });

  it('is inverse of compact for basic cases', () => {
    const categorical = { web: {}, api: {}, prod: {} };
    const compacted = compact(categorical);
    expect(compacted).toEqual(['web', 'api', 'prod']);
    expect(expand(compacted)).toEqual(categorical);
  });
});
