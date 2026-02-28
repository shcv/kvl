import { KvlParseError } from './errors.js';

/**
 * Configuration for KVL parsing and serialization.
 */
export class KvlConfig {
  /**
   * @param {object} [opts]
   * @param {string} [opts.separator='=']
   * @param {string} [opts.version='1.0']
   * @param {boolean} [opts.compact=false]
   * @param {boolean} [opts.spaceBefore=true]
   * @param {boolean} [opts.spaceAfter=true]
   * @param {string} [opts.listMarkers='']
   */
  constructor(opts = {}) {
    this.separator = opts.separator ?? '=';
    this.version = opts.version ?? '1.0';
    this.compact = opts.compact ?? false;
    this.spaceBefore = opts.spaceBefore ?? true;
    this.spaceAfter = opts.spaceAfter ?? true;
    this.listMarkers = opts.listMarkers ?? '';

    if (!this.separator) {
      throw new Error('Separator cannot be empty');
    }
    if (/[\s]/.test(this.separator)) {
      throw new Error('Separator cannot contain whitespace characters');
    }
  }
}

/**
 * Create a KvlConfig with automatically configured spacing for the separator.
 * @param {string} separator
 * @param {object} [overrides]
 * @returns {KvlConfig}
 */
export function autoConfigForSeparator(separator, overrides = {}) {
  const defaults = separator === ':'
    ? { spaceBefore: false, spaceAfter: true }
    : { spaceBefore: true, spaceAfter: true };

  return new KvlConfig({ ...defaults, separator, ...overrides });
}

/**
 * Parse KVL header from the first line of text.
 *
 * Format: #<separator> kvl <version> [list_markers] [options]
 *
 * @param {string} text
 * @returns {KvlConfig|null}
 */
export function parseHeader(text) {
  const lines = text.split('\n');
  if (!lines.length) return null;

  const firstLine = lines[0].trim();
  if (!firstLine.startsWith('#')) return null;

  const m = firstLine.match(/^#(.+?)\s+kvl(?:\s+(.*))?$/);
  if (!m) return null;

  const separator = m[1];
  const rest = m[2] || '';
  const parts = rest.split(/\s+/).filter(Boolean);

  if (!parts.length) {
    throw new KvlParseError('Missing version in KVL header');
  }

  const version = parts[0];
  let listMarkers = '';
  let optionParts;

  if (parts.length > 1) {
    const second = parts[1];
    if (!second.includes('=') && !/^[a-zA-Z0-9]+$/.test(second)) {
      listMarkers = second;
      optionParts = parts.slice(2);
    } else {
      optionParts = parts.slice(1);
    }
  } else {
    optionParts = [];
  }

  const options = {};
  for (const part of optionParts) {
    if (part.includes('=')) {
      const [key, value] = part.split('=', 2);
      options[normalizeKey(key)] = coerceValue(value);
    } else {
      options[normalizeKey(part)] = true;
    }
  }

  if ('spaceAround' in options) {
    const around = options.spaceAround;
    delete options.spaceAround;
    if (!('spaceBefore' in options)) options.spaceBefore = around;
    if (!('spaceAfter' in options)) options.spaceAfter = around;
  }

  if (!('spaceBefore' in options) && !('spaceAfter' in options)) {
    return autoConfigForSeparator(separator, {
      version,
      compact: options.compact ?? false,
      listMarkers,
    });
  }

  const auto = autoConfigForSeparator(separator);
  return new KvlConfig({
    separator,
    version,
    compact: options.compact ?? false,
    spaceBefore: options.spaceBefore ?? auto.spaceBefore,
    spaceAfter: options.spaceAfter ?? auto.spaceAfter,
    listMarkers,
  });
}

/**
 * Generate KVL header line from configuration.
 * @param {KvlConfig} config
 * @returns {string}
 */
export function generateHeader(config) {
  const parts = [`#${config.separator} kvl ${config.version}`];
  if (config.listMarkers) parts.push(config.listMarkers);
  if (config.compact) parts.push('compact');
  return parts.join(' ');
}

/**
 * Extract content from KVL text, removing header if present.
 * @param {string} text
 * @returns {string}
 */
export function extractContent(text) {
  const lines = text.split('\n');
  if (!lines.length) return text;

  const firstLine = lines[0].trim();
  if (firstLine.startsWith('#') && firstLine.includes(' kvl ')) {
    return lines.slice(1).join('\n');
  }
  return text;
}

/** @param {string} key */
function normalizeKey(key) {
  // Convert kebab-case to camelCase
  return key.replace(/-([a-z])/g, (_, c) => c.toUpperCase());
}

/** @param {*} value */
function coerceValue(value) {
  if (typeof value === 'string') {
    const lower = value.toLowerCase();
    if (lower === 'true') return true;
    if (lower === 'false') return false;
  }
  return value;
}
