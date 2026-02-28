/**
 * Parser for Key-Value Language (KVL).
 *
 * Based on the original Categorical Configuration Language (CCL) by Dmitrii Kovanikov.
 */

import { readFileSync } from 'node:fs';
import { KvlParseError } from './errors.js';
import { KvlConfig, parseHeader, extractContent } from './config.js';
import { merge, compact } from './transform.js';

const MAX_RECURSION_DEPTH = 100;
const MAX_INPUT_SIZE = 10 * 1024 * 1024; // 10 MB

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
  return compact(raw, config);
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
      valueLines.push(line);
      i++;
      continue;
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
      key = line.trim();
      valuePart = '';
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
 * Find the first separator not preceded by a backslash.
 * @param {string} line
 * @param {string} separator
 * @returns {number} index or -1
 */
function _findUnescapedSeparator(line, separator) {
  let pos = 0;
  while (pos < line.length) {
    const sepPos = line.indexOf(separator, pos);
    if (sepPos === -1) return -1;
    if (sepPos > 0 && line[sepPos - 1] === '\\') {
      pos = sepPos + 1;
    } else {
      return sepPos;
    }
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

  const hasSeparator = value.includes(config.separator);
  const hasListMarkers = config.listMarkers && value.split('\n').some(line => {
    const trimmed = line.trimStart();
    return config.listMarkers.split('').some(
      marker => trimmed.startsWith(marker + ' ') || trimmed.startsWith(marker + '\t')
    );
  });

  if (!hasSeparator && !hasListMarkers) {
    return { [value]: {} };
  }

  const nestedKvs = _parseKvs(value, config, true, depth + 1);
  return _buildModel(nestedKvs, config, depth);
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
 * @param {string} text
 * @param {string} separator
 * @returns {string}
 */
function _unescapeText(text, separator) {
  return text.replaceAll('\\' + separator, separator);
}
