import { describe, it, expect } from 'vitest';
import {
  KvlConfig,
  autoConfigForSeparator,
  parseHeader,
  generateHeader,
  extractContent,
  KvlParseError,
} from '../src/index.js';

describe('KvlConfig', () => {
  it('creates default config', () => {
    const config = new KvlConfig();
    expect(config.separator).toBe('=');
    expect(config.version).toBe('1.0');
    expect(config.spaceBefore).toBe(true);
    expect(config.spaceAfter).toBe(true);
    expect(config.listMarkers).toBe('');
    expect(config.compact).toBe(false);
  });

  it('accepts custom options', () => {
    const config = new KvlConfig({ separator: ':', spaceBefore: false, listMarkers: '-+*' });
    expect(config.separator).toBe(':');
    expect(config.spaceBefore).toBe(false);
    expect(config.listMarkers).toBe('-+*');
  });

  it('rejects empty separator', () => {
    expect(() => new KvlConfig({ separator: '' })).toThrow();
  });

  it('rejects whitespace in separator', () => {
    expect(() => new KvlConfig({ separator: '= ' })).toThrow();
    expect(() => new KvlConfig({ separator: '\t' })).toThrow();
  });
});

describe('autoConfigForSeparator', () => {
  it('uses space around for most separators', () => {
    const config = autoConfigForSeparator('=');
    expect(config.spaceBefore).toBe(true);
    expect(config.spaceAfter).toBe(true);
  });

  it('uses no space before colon', () => {
    const config = autoConfigForSeparator(':');
    expect(config.spaceBefore).toBe(false);
    expect(config.spaceAfter).toBe(true);
  });

  it('accepts overrides', () => {
    const config = autoConfigForSeparator(':', { spaceBefore: true, listMarkers: '-' });
    expect(config.spaceBefore).toBe(true);
    expect(config.listMarkers).toBe('-');
  });
});

describe('parseHeader', () => {
  it('returns null for non-header text', () => {
    expect(parseHeader('key = value')).toBeNull();
    expect(parseHeader('')).toBeNull();
  });

  it('parses basic header', () => {
    const config = parseHeader('#= kvl 1.0');
    expect(config.separator).toBe('=');
    expect(config.version).toBe('1.0');
  });

  it('parses header with list markers', () => {
    const config = parseHeader('#= kvl 1.0 -');
    expect(config.listMarkers).toBe('-');
  });

  it('parses header with multiple list markers', () => {
    const config = parseHeader('#: kvl 1.0 -+*');
    expect(config.separator).toBe(':');
    expect(config.listMarkers).toBe('-+*');
  });

  it('parses header with arrow separator', () => {
    const config = parseHeader('#-> kvl 1.0');
    expect(config.separator).toBe('->');
  });

  it('throws on missing version', () => {
    expect(() => parseHeader('#= kvl')).toThrow(KvlParseError);
  });

  it('parses compact flag', () => {
    const config = parseHeader('#= kvl 1.0 compact');
    expect(config.compact).toBe(true);
  });
});

describe('generateHeader', () => {
  it('generates basic header', () => {
    const config = new KvlConfig();
    expect(generateHeader(config)).toBe('#= kvl 1.0');
  });

  it('includes list markers', () => {
    const config = new KvlConfig({ listMarkers: '-' });
    expect(generateHeader(config)).toBe('#= kvl 1.0 -');
  });

  it('includes compact flag', () => {
    const config = new KvlConfig({ compact: true });
    expect(generateHeader(config)).toBe('#= kvl 1.0 compact');
  });

  it('generates with custom separator', () => {
    const config = new KvlConfig({ separator: '->' });
    expect(generateHeader(config)).toBe('#-> kvl 1.0');
  });
});

describe('extractContent', () => {
  it('removes header line', () => {
    expect(extractContent('#= kvl 1.0\nkey = value')).toBe('key = value');
  });

  it('returns text unchanged if no header', () => {
    expect(extractContent('key = value')).toBe('key = value');
  });

  it('handles empty text', () => {
    expect(extractContent('')).toBe('');
  });
});
