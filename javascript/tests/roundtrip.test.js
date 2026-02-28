import { describe, it, expect } from 'vitest';
import { loads, dumps, parse, compact } from '../src/index.js';

describe('Round-trip: parse -> serialize -> parse', () => {
  it('roundtrips simple values', () => {
    const original = 'name = test\nversion = 1.0\nactive = true';
    const data1 = loads(original);
    const serialized = dumps(data1);
    expect(compact(parse(serialized))).toEqual(data1);
  });

  it('roundtrips nested structures', () => {
    const original = `server =\n  host = localhost\n  port = 8080\ndatabase =\n  url = postgres://localhost\n  pool = 10`;
    const data1 = loads(original);
    const serialized = dumps(data1);
    expect(compact(parse(serialized))).toEqual(data1);
  });

  it('roundtrips lists (repeated keys)', () => {
    const original = 'tags = web\ntags = api\ntags = production';
    const data1 = loads(original);
    expect(data1).toEqual({ tags: ['web', 'api', 'production'] });
    const serialized = dumps(data1);
    const data2 = loads(serialized);
    expect(data2).toEqual(data1);
    expect(data2.tags).toEqual(['web', 'api', 'production']);
  });

  it('roundtrips duplicate list items', () => {
    const original = 'tags = web\ntags = api\ntags = web\ntags = production';
    const data1 = loads(original);
    const serialized = dumps(data1);
    const data2 = loads(serialized);
    expect(data1).toEqual(data2);
  });

  it('roundtrips comment keys', () => {
    const original = '/= This is a comment\nname = value\n/= Another comment\nversion = 1.0';
    const data1 = loads(original);
    // /= lines merge categorically under the "/" key
    expect(data1['/']).toEqual(['This is a comment', 'Another comment']);
    const serialized = dumps(data1);
    const data2 = loads(serialized);
    expect(data1).toEqual(data2);
  });

  it('roundtrips escaped separators', () => {
    const original = 'equation = x\\=y+z';
    const data1 = loads(original);
    expect(data1.equation).toBe('x=y+z');
    const serialized = dumps(data1);
    const data2 = loads(serialized);
    expect(data1).toEqual(data2);
  });

  it('roundtrips complex nested with lists', () => {
    const original = `config =\n  server =\n    ports = 80\n    ports = 443\n    hosts = web1\n    hosts = web2\n  features =\n    auth = enabled\n    logging = enabled`;
    const data1 = loads(original);
    const serialized = dumps(data1);
    const data2 = loads(serialized);
    expect(data1).toEqual(data2);
    expect(data2.config.server.ports).toEqual(['80', '443']);
    expect(data2.config.server.hosts).toEqual(['web1', 'web2']);
  });

  it('roundtrips empty values / anonymous lists', () => {
    const original = 'items =\n  = first\n  = second\n  = third';
    const data1 = loads(original);
    expect(data1).toEqual({ items: ['first', 'second', 'third'] });
    const serialized = dumps(data1);
    const data2 = loads(serialized);
    expect(data1).toEqual(data2);
  });

  it('roundtrips mixed flat and nested', () => {
    const original = 'name = MyApp\nversion = 1.0\nserver =\n  host = localhost\n  port = 8080\ntags = web\ntags = api\ndebug = true';
    const data1 = loads(original);
    const serialized = dumps(data1);
    const data2 = loads(serialized);
    expect(data1).toEqual(data2);
  });

  it('roundtrips unicode content', () => {
    const original = 'emoji = 🚀💻\naccents = café résumé\nchinese = 你好世界';
    const data1 = loads(original);
    const serialized = dumps(data1);
    const data2 = loads(serialized);
    expect(data1).toEqual(data2);
    expect(data2.emoji).toBe('🚀💻');
    expect(data2.chinese).toBe('你好世界');
  });
});
