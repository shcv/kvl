/**
 * Serializer for Key-Value Language (KVL).
 */

import { writeFileSync } from 'node:fs';
import { KvlSerializeError } from './errors.js';
import { KvlConfig, generateHeader } from './config.js';
import { expand } from './transform.js';

/**
 * Serialize data to KVL format.
 *
 * @param {object} data
 * @param {KvlConfig} [config]
 * @param {object} [options]
 * @param {boolean} [options.includeHeader=false]
 * @param {string|null} [options.listMarker]  Explicit marker or null for auto-detect
 * @param {boolean} [options.publicFormat=false]
 * @param {string} [options.indent='  ']
 * @returns {string}
 */
export function dumps(data, config, options = {}) {
  config = config ?? new KvlConfig();
  const { includeHeader = false, listMarker, publicFormat = false, indent = '  ' } = options;

  if (!data || Object.keys(data).length === 0) {
    return includeHeader ? generateHeader(config) + '\n' : '';
  }

  const effectiveMarker = _determineListMarker(listMarker, config);

  let serializeData;
  if (effectiveMarker || publicFormat) {
    serializeData = data;
  } else {
    serializeData = expand(data);
  }

  const serializer = new KvlSerializer(serializeData, config, indent, effectiveMarker, publicFormat);
  let result = serializer.serialize();

  if (includeHeader) {
    result = generateHeader(config) + '\n' + result;
  }

  return result;
}

/**
 * Serialize data to KVL format and write to a file.
 *
 * @param {object} data
 * @param {string} filePath
 * @param {KvlConfig} [config]
 * @param {object} [options]
 */
export function dump(data, filePath, config, options = {}) {
  const serialized = dumps(data, config, options);
  writeFileSync(filePath, serialized, 'utf-8');
}

function isScalar(value) {
  return value == null || typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean';
}

// ---------------------------------------------------------------------------
// Internal
// ---------------------------------------------------------------------------

/**
 * @param {string|null|undefined} listMarker
 * @param {KvlConfig} config
 * @returns {string|null}
 */
function _determineListMarker(listMarker, config) {
  if (listMarker !== undefined && listMarker !== null) {
    return listMarker || null;
  }
  if (config.listMarkers) return config.listMarkers[0];
  return null;
}

class KvlSerializer {
  /**
   * @param {object} data
   * @param {KvlConfig} config
   * @param {string} indent
   * @param {string|null} listMarker
   * @param {boolean} publicFormat
   */
  constructor(data, config, indent = '  ', listMarker = null, publicFormat = false) {
    this.data = data;
    this.config = config;
    this.indent = indent;
    this.listMarker = listMarker;
    this.publicFormat = publicFormat;
    /** @type {string[]} */
    this.lines = [];
  }

  serialize() {
    if (!this.data || Object.keys(this.data).length === 0) return '';
    this._serializeDict(this.data, 0);
    let result = this.lines.join('\n');
    if (result) result += '\n';
    return result;
  }

  /** @param {string} text */
  _escapeText(text) {
    if (!text || !this.config.separator) return text;
    return text.replaceAll(this.config.separator, '\\' + this.config.separator);
  }

  /**
   * @param {boolean} [forEmpty=false]
   * @param {boolean} [forMultiline=false]
   */
  _formatSeparator(forEmpty = false, forMultiline = false) {
    const before = this.config.spaceBefore ? ' ' : '';
    let after;
    if (forEmpty || forMultiline) {
      after = '';
    } else {
      after = this.config.spaceAfter ? ' ' : '';
    }
    return `${before}${this.config.separator}${after}`;
  }

  /**
   * @param {object} data
   * @param {number} level
   */
  _serializeDict(data, level) {
    const indentStr = this.indent.repeat(level);

    for (const [key, value] of Object.entries(data)) {
      if (typeof key !== 'string') {
        throw new KvlSerializeError(`Keys must be strings, got ${typeof key}`);
      }

      if (value !== null && typeof value === 'object' && !Array.isArray(value)) {
        this._serializeDictValue(key, value, indentStr, level);
      } else if (Array.isArray(value)) {
        this._serializeListValue(key, value, indentStr, level);
      } else if (isScalar(value)) {
        this._serializeScalarValue(key, value, indentStr, level);
      } else {
        throw new KvlSerializeError(`Unsupported value type: ${typeof value}`);
      }
    }
  }

  _serializeDictValue(key, value, indentStr, level) {
    const sep = this._formatSeparator(true);
    const escapedKey = this._escapeText(key);
    this.lines.push(`${indentStr}${escapedKey}${sep}`);
    if (Object.keys(value).length) {
      this._serializeDict(value, level + 1);
    }
  }

  _serializeListValue(key, value, indentStr, level) {
    if (!this.listMarker) {
      if (this.publicFormat) {
        for (const item of value) {
          if (Array.isArray(item) || (item !== null && typeof item === 'object')) {
            throw new KvlSerializeError(
              'Complex list items require a configured list marker in publicFormat mode.'
            );
          }
          this._serializeScalarValue(key, item, indentStr, level);
        }
        return;
      }
      throw new KvlSerializeError(
        'List values are not supported without list markers. Use expand() to convert to categorical format.'
      );
    }
    const sep = this._formatSeparator(true);
    const escapedKey = this._escapeText(key);
    this.lines.push(`${indentStr}${escapedKey}${sep}`);
    this._serializeList(value, level + 1);
  }

  _serializeScalarValue(key, value, indentStr, level = 0) {
    const text = value == null ? 'null' : String(value);
    this._serializeStringValue(key, text, indentStr, level);
  }

  _serializeStringValue(key, value, indentStr, level = 0) {
    const escapedKey = this._escapeText(key);
    if (!value) {
      const sep = this._formatSeparator(true);
      this.lines.push(`${indentStr}${escapedKey}${sep}`);
    } else if (value.includes('\n')) {
      this._serializeMultilineValue(escapedKey, value, indentStr, level);
    } else {
      const escapedValue = this._escapeText(value);
      const sep = this._formatSeparator();
      this.lines.push(`${indentStr}${escapedKey}${sep}${escapedValue}`);
    }
  }

  _serializeMultilineValue(escapedKey, value, indentStr, level) {
    const parentIndentLen = indentStr.length;

    if (value.startsWith('\n')) {
      // Empty-key continuation: value is \n followed by indented lines
      const contentLines = value.slice(1).split('\n');
      const nonBlank = contentLines.filter(l => l.trim());
      const minIndent = nonBlank.length
        ? Math.min(...nonBlank.map(l => l.length - l.trimStart().length))
        : 0;
      if (nonBlank.length && minIndent > parentIndentLen) {
        // Original indentation is valid — preserve it
        const sep = this._formatSeparator(true);
        this.lines.push(`${indentStr}${escapedKey}${sep}`);
        for (const line of contentLines) {
          this.lines.push(line);
        }
      } else {
        // Re-indent canonically
        const sep = this._formatSeparator(true);
        this.lines.push(`${indentStr}${escapedKey}${sep}`);
        const childIndent = this.indent.repeat(level + 1);
        for (const line of contentLines) {
          this.lines.push(`${childIndent}${line}`);
        }
      }
    } else {
      // Valued-key continuation: first line is inline, rest are indented
      const allLines = value.split('\n');
      const contLines = allLines.slice(1);
      const nonBlank = contLines.filter(l => l.trim());
      const minIndent = nonBlank.length
        ? Math.min(...nonBlank.map(l => l.length - l.trimStart().length))
        : 0;
      if (nonBlank.length && minIndent > parentIndentLen) {
        // Continuation lines have valid indentation — preserve as valued-key form
        const sep = this._formatSeparator();
        this.lines.push(`${indentStr}${escapedKey}${sep}${this._escapeText(allLines[0])}`);
        for (const line of contLines) {
          this.lines.push(line);
        }
      } else {
        // Re-indent canonically using empty-key form
        const sep = this._formatSeparator(true);
        this.lines.push(`${indentStr}${escapedKey}${sep}`);
        const childIndent = this.indent.repeat(level + 1);
        for (const line of allLines) {
          this.lines.push(`${childIndent}${line}`);
        }
      }
    }
  }

  /**
   * @param {Array} items
   * @param {number} level
   */
  _serializeList(items, level) {
    const indentStr = this.indent.repeat(level);
    const markerStr = `${this.listMarker} `;

    for (const item of items) {
      if (typeof item === 'string') {
        this.lines.push(`${indentStr}${markerStr}${this._escapeText(item)}`);
      } else if (Array.isArray(item)) {
        this.lines.push(`${indentStr}${this.listMarker}`);
        this._serializeList(item, level + 1);
      } else if (item !== null && typeof item === 'object' && !Array.isArray(item)) {
        const entries = Object.entries(item);
        if (entries.length !== 1) {
          this.lines.push(`${indentStr}${this.listMarker}`);
          this._serializeDict(item, level + 1);
          continue;
        }
        if (entries.length === 1) {
          const [k, v] = entries[0];
          if (typeof v === 'string' && !v.includes('\n')) {
            const sep = this._formatSeparator();
            this.lines.push(`${indentStr}${markerStr}${this._escapeText(k)}${sep}${this._escapeText(v)}`);
          } else {
            const sep = this._formatSeparator(true);
            this.lines.push(`${indentStr}${markerStr}${this._escapeText(k)}${sep}`);
            if (typeof v === 'object' && v !== null && !Array.isArray(v)) {
              this._serializeDict(v, level + 1);
            } else if (Array.isArray(v) && this.listMarker) {
              this._serializeList(v, level + 1);
            } else {
              this.lines.push(`${this.indent.repeat(level + 1)}${v}`);
            }
          }
        } else {
          let first = true;
          for (const [k, v] of entries) {
            if (first) {
              if (typeof v === 'string' && !v.includes('\n')) {
                const sep = this._formatSeparator();
                this.lines.push(`${indentStr}${markerStr}${this._escapeText(k)}${sep}${this._escapeText(v)}`);
              } else {
                const sep = this._formatSeparator(true);
                this.lines.push(`${indentStr}${markerStr}${this._escapeText(k)}${sep}`);
                if (typeof v === 'object' && v !== null) {
                  this._serializeDict(v, level + 1);
                }
              }
              first = false;
            } else {
              const nestedIndent = this.indent.repeat(level + 1);
              if (typeof v === 'string' && !v.includes('\n')) {
                const sep = this._formatSeparator();
                this.lines.push(`${nestedIndent}${this._escapeText(k)}${sep}${this._escapeText(v)}`);
              } else {
                const sep = this._formatSeparator(true);
                this.lines.push(`${nestedIndent}${this._escapeText(k)}${sep}`);
                if (typeof v === 'object' && v !== null) {
                  this._serializeDict(v, level + 2);
                }
              }
            }
          }
        }
      } else {
        this.lines.push(`${indentStr}${markerStr}${String(item)}`);
      }
    }
  }
}
