/**
 * Parser for Key-Value Language (KVL).
 *
 * Based on the original Categorical Configuration Language (CCL) by Dmitrii Kovanikov.
 */

import { readFileSync } from 'node:fs';
import { KvlParseError, KvlDiagnostic } from './errors.js';
import { KvlConfig, parseHeader, extractContent } from './config.js';
import { merge, compact } from './transform.js';

const MAX_RECURSION_DEPTH = 100;
const MAX_INPUT_SIZE = 10 * 1024 * 1024; // 10 MB

/**
 * Emit a diagnostic (warning, or error in strict mode).
 * @param {import('./config.js').KvlConfig} config
 * @param {string} code
 * @param {string} message
 * @param {number} [line]
 */
function _emitDiagnostic(config, code, message, line) {
  if (config.strict) {
    throw new KvlParseError(`[${code}] ${message}`, line);
  }
  config.diagnostics.push(new KvlDiagnostic('warning', code, message, line));
}

/**
 * Parse KVL text into a list of single-key dictionaries.
 * @param {string} text
 * @param {KvlConfig} [config]
 * @returns {Array<Object<string, string>>}
 */
export function keyvals(text, config) {
  config = ensureConfig(config, text);
  if (!text.trim()) return [];
  return _parseKvs(text, config);
}

/**
 * Low-level API: parse KVL text into raw categorical model.
 * Repeated keys appear as nested mappings.
 *
 * @param {string} text
 * @param {KvlConfig} [config]
 * @returns {object}
 */
export function parse(text, config) {
  if (text.length > MAX_INPUT_SIZE) {
    throw new KvlParseError(`Input exceeds maximum size of ${MAX_INPUT_SIZE} bytes`);
  }
  _checkIndentConsistency(text);
  config = ensureConfig(config, text);
  const kvs = keyvals(text, config);
  const model = _buildModel(kvs, config);
  return _unescapeModel(model, config);
}

/**
 * High-level API: parse KVL text and compact to user-friendly format.
 *
 * @param {string} text
 * @param {KvlConfig} [config]
 * @returns {*}
 */
export function loads(text, config) {
  config = ensureConfig(config, text);
  const raw = parse(text, config);
  const compacted = compact(raw, config);
  return _trimMultilineValues(compacted);
}

/**
 * Parse KVL from a file path.
 * @param {string} filePath
 * @param {KvlConfig} [config]
 * @returns {*}
 */
export function load(filePath, config) {
  const text = readFileSync(filePath, 'utf-8');
  return loads(text, config);
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/**
 * @param {KvlConfig|null|undefined} config
 * @param {string} text
 * @returns {KvlConfig}
 */
function ensureConfig(config, text) {
  if (config) return config;
  return parseHeader(text) ?? new KvlConfig();
}

/**
 * Trim a multiline string value for loads() output.
 * @param {string} value
 * @returns {string}
 */
function _trimMultiline(value) {
  if (!value.includes('\n')) return value;

  if (value.startsWith('\n')) {
    // Empty-key continuation: strip leading \n, dedent all lines
    value = value.slice(1);
    const lines = value.split('\n');
    const nonBlank = lines.filter(l => l.trim());
    if (!nonBlank.length) return value;
    const minIndent = Math.min(...nonBlank.map(l => l.length - l.trimStart().length));
    if (minIndent === 0) return value;
    return lines.map(l => l.slice(minIndent)).join('\n');
  }

  // Valued-key continuation: first line is inline, rest are continuation
  const lines = value.split('\n');
  if (lines.length < 2) return value;
  const contLines = lines.slice(1);
  const nonBlankCont = contLines.filter(l => l.trim());
  if (!nonBlankCont.length) return value;
  const minIndent = Math.min(...nonBlankCont.map(l => l.length - l.trimStart().length));
  if (minIndent === 0) return value;
  return [lines[0], ...contLines.map(l => l.slice(minIndent))].join('\n');
}

/**
 * Walk a compacted data structure and trim multiline string values.
 * @param {*} data
 * @returns {*}
 */
function _trimMultilineValues(data) {
  if (typeof data === 'string') return _trimMultiline(data);
  if (Array.isArray(data)) return data.map(item => _trimMultilineValues(item));
  if (data !== null && typeof data === 'object') {
    const result = {};
    for (const [k, v] of Object.entries(data)) {
      result[k] = _trimMultilineValues(v);
    }
    return result;
  }
  return data;
}

/**
 * Verify that indentation uses only tabs or only spaces, not both.
 * The first indented line determines the indent mode.
 * @param {string} text
 */
function _checkIndentConsistency(text) {
  let indentChar = null;
  const lines = text.split('\n');
  for (let lineNum = 0; lineNum < lines.length; lineNum++) {
    const line = lines[lineNum];
    if (!line || (line[0] !== ' ' && line[0] !== '\t')) continue;
    // Extract leading whitespace
    let leadingEnd = 0;
    while (leadingEnd < line.length && (line[leadingEnd] === ' ' || line[leadingEnd] === '\t')) {
      leadingEnd++;
    }
    if (leadingEnd === 0) continue;
    if (indentChar === null) {
      indentChar = line[0];
    }
    for (let col = 0; col < leadingEnd; col++) {
      if (line[col] !== indentChar) {
        const expected = indentChar === '\t' ? 'tabs' : 'spaces';
        const found = line[col] === '\t' ? 'tab' : 'space';
        throw new KvlParseError(
          `Mixed indentation: expected ${expected} but found ${found}`,
          lineNum + 1, col + 1
        );
      }
    }
  }
}

/**
 * Split text into lines, matching Python's splitlines() behavior
 * (trailing newline does not create an extra empty element).
 * @param {string} text
 * @returns {string[]}
 */
function splitLines(text) {
  const lines = text.split('\n');
  if (lines.length > 0 && lines[lines.length - 1] === '') {
    lines.pop();
  }
  return lines;
}

/**
 * Collect continuation lines that are more indented than parentIndent.
 * @param {string[]} lines
 * @param {number} startIdx
 * @param {number} parentIndent
 * @returns {[string, number]} [value, newIndex]
 */
function _collectMultilineValue(lines, startIdx, parentIndent) {
  const valueLines = [];
  let i = startIdx;
  while (i < lines.length) {
    const line = lines[i];
    if (!line.trim()) {
      // Peek ahead: only include blank lines if more indented content follows
      let j = i + 1;
      while (j < lines.length && !lines[j].trim()) j++;
      if (j < lines.length) {
        const nextIndent = lines[j].length - lines[j].trimStart().length;
        if (nextIndent > parentIndent) {
          for (let k = i; k < j; k++) valueLines.push(lines[k]);
          i = j;
          continue;
        }
      }
      break;
    }
    if (line.length - line.trimStart().length <= parentIndent) break;
    valueLines.push(line);
    i++;
  }
  return [valueLines.length ? '\n' + valueLines.join('\n') : '', i];
}

/**
 * Core key-value parser.
 * @param {string} text
 * @param {KvlConfig} config
 * @param {boolean} [allowAnonymousLists=false]
 * @param {number} [depth=0]
 * @returns {Array<Object<string, string>>}
 */
function _parseKvs(text, config, allowAnonymousLists = false, depth = 0) {
  if (depth > MAX_RECURSION_DEPTH) {
    throw new KvlParseError(`Maximum nesting depth of ${MAX_RECURSION_DEPTH} exceeded`);
  }

  const content = extractContent(text);
  const lines = splitLines(content);
  const result = [];
  let i = 0;
  let currentListKey = null;
  const anonymousListItems = [];

  while (i < lines.length) {
    const line = lines[i];
    if (!line.trim()) { i++; continue; }

    const firstCharPos = line.length - line.trimStart().length;

    // List marker handling
    if (firstCharPos < line.length && _isListMarker(line, firstCharPos, config)) {
      const listItemContent = line.slice(firstCharPos + 1).trimStart();
      const itemSepPos = _findUnescapedSeparator(listItemContent, config.separator);

      if (currentListKey) {
        result.push({ [currentListKey]: listItemContent });
      } else if (allowAnonymousLists) {
        if (itemSepPos !== -1) {
          const itemKey = listItemContent.slice(0, itemSepPos).trim();
          let itemValue = listItemContent.slice(itemSepPos + config.separator.length).trim();

          if (!itemValue && i + 1 < lines.length) {
            const nextLine = lines[i + 1] || '';
            if (nextLine && (nextLine.startsWith(' ') || nextLine.startsWith('\t'))) {
              const parentIndent = line.length - line.trimStart().length;
              const [mlValue, newI] = _collectMultilineValue(lines, i + 1, parentIndent);
              itemValue = mlValue;
              i = newI - 1;
            }
          }
          anonymousListItems.push({ [itemKey]: itemValue });
        } else {
          anonymousListItems.push(listItemContent);
        }
      } else {
        throw new KvlParseError(`List item found without preceding key at line ${i + 1}`);
      }
      i++;
      continue;
    }

    // Normal line parsing
    const lineIndent = line.length - line.trimStart().length;
    const sepPos = _findUnescapedSeparator(line, config.separator);

    // Multiline continuation (indented line without separator)
    if (sepPos === -1 && lineIndent > 0 && result.length) {
      const lastEntry = result[result.length - 1];
      const lastKey = Object.keys(lastEntry)[0];
      if (lastEntry[lastKey]) {
        lastEntry[lastKey] += '\n' + line.trimEnd();
      } else {
        lastEntry[lastKey] = '\n' + line.trimEnd();
      }
      i++;
      continue;
    }

    let key, valuePart;
    if (sepPos === -1) {
      throw new KvlParseError(`Missing separator '${config.separator}'`, i + 1);
    } else {
      key = line.slice(0, sepPos).trim();
      valuePart = line.slice(sepPos + config.separator.length).trim();
    }

    if (!valuePart) {
      currentListKey = key;
    } else {
      currentListKey = null;
    }

    // Multiline value collection
    if (!valuePart && i + 1 < lines.length) {
      const nextLine = lines[i + 1] || '';
      if (nextLine && (nextLine.startsWith(' ') || nextLine.startsWith('\t'))) {
        const parentIndent = line.length - line.trimStart().length;
        const [mlValue, newI] = _collectMultilineValue(lines, i + 1, parentIndent);
        valuePart = mlValue;
        i = newI - 1;
      }
    } else if (valuePart && i + 1 < lines.length) {
      // Valued key with indented children → continuation text (W001)
      const nextLine = lines[i + 1] || '';
      if (nextLine && (nextLine.startsWith(' ') || nextLine.startsWith('\t'))) {
        const nextIndent = nextLine.length - nextLine.trimStart().length;
        const parentIndent = line.length - line.trimStart().length;
        if (nextIndent > parentIndent) {
          const [contValue, newI] = _collectMultilineValue(lines, i + 1, parentIndent);
          if (contValue) {
            _emitDiagnostic(config, 'W001',
              'Valued key with continuation: indented lines after ' +
              'a valued key are treated as continuation text',
              i + 1);
            valuePart = valuePart + contValue;
            i = newI - 1;
          }
        }
      }
    }

    result.push({ [key]: valuePart });
    i++;
  }

  // Append anonymous list items as tagged entries
  if (anonymousListItems.length && allowAnonymousLists) {
    for (const item of anonymousListItems) {
      if (typeof item === 'object' && !Array.isArray(item)) {
        result.push({ _anonType: 'dict', _value: item });
      } else {
        result.push({ _anonType: 'list', _value: item });
      }
    }
  }

  return result;
}

/**
 * @param {string} line
 * @param {number} pos
 * @param {KvlConfig} config
 * @returns {boolean}
 */
function _isListMarker(line, pos, config) {
  if (!config.listMarkers || pos >= line.length) return false;
  return (
    config.listMarkers.includes(line[pos]) &&
    pos + 1 < line.length &&
    (line[pos + 1] === ' ' || line[pos + 1] === '\t')
  );
}

/**
 * Find the first separator not immediately preceded by a backslash.
 *
 * Simple escape rule: a backslash directly before the separator escapes it.
 * No pair processing — \\= means the second backslash escapes the =,
 * regardless of how many backslashes precede it.
 *
 * @param {string} line
 * @param {string} separator
 * @returns {number} index or -1
 */
function _findUnescapedSeparator(line, separator) {
  const sepLen = separator.length;
  let pos = 0;
  while (pos <= line.length - sepLen) {
    const sepPos = line.indexOf(separator, pos);
    if (sepPos === -1) return -1;
    if (sepPos > 0 && line[sepPos - 1] === '\\') {
      // Backslash immediately before separator = escaped
      pos = sepPos + 1;
      continue;
    }
    return sepPos;
  }
  return -1;
}

/**
 * Process a single value into a categorical model.
 * @param {string} value
 * @param {KvlConfig} config
 * @param {number} [depth=0]
 * @returns {object}
 */
function _processValue(value, config, depth = 0) {
  if (!value || !value.trim()) return {};

  // Only re-parse as nested KVL if the value is multiline (from indented
  // blocks).  Single-line values are always literal leaves — they must NOT
  // be re-split on the separator.
  if (!value.includes('\n')) {
    return { [value]: {} };
  }

  // Find base level = minimum indent among non-blank lines
  const lines = value.split('\n');
  const nonBlank = lines.filter(l => l.trim());
  if (!nonBlank.length) return { [value]: {} };

  const minIndent = Math.min(...nonBlank.map(l => l.length - l.trimStart().length));

  // Collect base-level lines (at minimum indent)
  const baseLines = nonBlank.filter(l => l.length - l.trimStart().length === minIndent);

  // Check how many base-level lines have separators or list markers
  let linesWithSep = 0;
  for (const line of baseLines) {
    const content = line.trimStart();
    const hasSep = _findUnescapedSeparator(content, config.separator) !== -1;
    const hasMarker = config.listMarkers && _isListMarker(content, 0, config);
    if (hasSep || hasMarker) linesWithSep++;
  }

  if (linesWithSep === baseLines.length) {
    // ALL base-level lines have separators → nested KVL
    const nestedKvs = _parseKvs(value, config, true, depth + 1);
    return _buildModel(nestedKvs, config, depth + 1);
  }

  if (linesWithSep === 0) {
    // NONE have separators → plain text (no warning)
    return { [value]: {} };
  }

  // SOME have separators → plain text + W002 warning
  _emitDiagnostic(config, 'W002',
    'Mixed continuation content: some base-level lines have separators ' +
    'but not all; block treated as plain text');
  return { [value]: {} };
}

/**
 * Convert list of key-value pairs to categorical model.
 * @param {Array<Object<string, string>>} keyVals
 * @param {KvlConfig} config
 * @param {number} [depth=0]
 * @returns {object|Array}
 */
function _buildModel(keyVals, config, depth = 0) {
  const anonymous = [];
  const regularKvs = [];

  for (const kv of keyVals) {
    if (kv._anonType) {
      anonymous.push(kv);
    } else {
      regularKvs.push(kv);
    }
  }

  // Only anonymous items → list
  if (anonymous.length && !regularKvs.length) {
    return anonymous.map(entry =>
      entry._anonType === 'list'
        ? _processValue(entry._value, config)
        : _buildModel([entry._value], config)
    );
  }

  // Group by key, process values
  const keyGroups = {};
  for (const kv of regularKvs) {
    for (const [key, value] of Object.entries(kv)) {
      if (!(key in keyGroups)) keyGroups[key] = [];
      keyGroups[key].push(value);
    }
  }

  const resultData = {};
  for (const [key, values] of Object.entries(keyGroups)) {
    const processed = values.map(v => _processValue(v, config, depth));
    if (processed.length) {
      let merged = processed[0];
      for (let j = 1; j < processed.length; j++) {
        merged = merge(merged, processed[j]);
      }
      resultData[key] = merged;
    }
  }

  return resultData;
}

/**
 * Recursively unescape all keys and values in the model.
 * @param {*} model
 * @param {KvlConfig} config
 * @returns {*}
 */
function _unescapeModel(model, config) {
  if (Array.isArray(model)) {
    return model.map(item => _unescapeModel(item, config));
  }
  if (model !== null && typeof model === 'object') {
    const result = {};
    for (const [k, v] of Object.entries(model)) {
      result[_unescapeText(k, config.separator)] = _unescapeModel(v, config);
    }
    return result;
  }
  return model;
}

/**
 * Unescape separator patterns: \<sep> → <sep>.
 * Scans left-to-right: a backslash immediately followed by the separator
 * is consumed as an escape (producing just the separator).  All other
 * backslashes are literal.
 * @param {string} text
 * @param {string} separator
 * @returns {string}
 */
function _unescapeText(text, separator) {
  const escaped = '\\' + separator;
  if (!text.includes(escaped)) return text;

  const parts = [];
  const sepLen = separator.length;
  let i = 0;
  while (i < text.length) {
    if (i < text.length - sepLen
        && text[i] === '\\'
        && text.slice(i + 1, i + 1 + sepLen) === separator) {
      parts.push(separator);
      i += 1 + sepLen;
    } else {
      parts.push(text[i]);
      i++;
    }
  }
  return parts.join('');
}
