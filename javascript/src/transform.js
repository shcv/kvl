import { KvlConfig } from './config.js';

/**
 * Merge two categorical models recursively.
 *
 * When both are dicts → recursive merge.
 * Conflicting scalars → convert to categorical and merge.
 *
 * @param {*} model1
 * @param {*} model2
 * @returns {*}
 */
export function merge(model1, model2) {
  if (isDict(model1) && isDict(model2)) {
    const result = { ...model1 };
    for (const [k, v] of Object.entries(model2)) {
      result[k] = k in result ? merge(result[k], v) : v;
    }
    return result;
  }

  if (isDict(model1)) return model1;
  if (isDict(model2)) return model2;

  // Both non-dict → convert to categorical and merge
  const cat1 = model1 != null && model1 !== '' ? { [String(model1)]: {} } : {};
  const cat2 = model2 != null && model2 !== '' ? { [String(model2)]: {} } : {};
  return merge(cat1, cat2);
}

/**
 * Compact KVL structures:
 *  1. Singleton empty key lifting: {'': content} → content
 *  2. Single empty child → string: {'value': {}} → 'value'
 *  3. All-empty-dict flattening: {'a': {}, 'b': {}} → ['a', 'b']
 *
 * @param {*} data
 * @param {KvlConfig} [config]
 * @returns {*}
 */
export function compact(data, config) {
  config = config ?? new KvlConfig();

  if (Array.isArray(data)) {
    return data.map(item => compact(item, config));
  }
  if (!isDict(data)) return data;

  // Recursively compact nested structures first
  const compacted = {};
  for (const [key, value] of Object.entries(data)) {
    compacted[key] = compact(value, config);
  }

  return applyCompactingRules(compacted, config);
}

/**
 * Expand lists back to categorical dict format for serialization.
 * Inverse of compact().
 *
 * @param {*} data
 * @returns {*}
 */
export function expand(data) {
  if (Array.isArray(data)) {
    const result = {};
    for (const item of data) {
      if (typeof item === 'string') {
        result[item] = {};
      } else {
        const expanded = expand(item);
        if ('' in result) {
          result[''] = merge(result[''], expanded);
        } else {
          result[''] = expanded;
        }
      }
    }
    return result;
  }

  if (isDict(data)) {
    const result = {};
    for (const [key, value] of Object.entries(data)) {
      result[key] = expand(value);
    }
    return result;
  }

  // Primitive → categorical
  if (data == null) return {};
  return { [String(data)]: {} };
}

/**
 * @param {object} data
 * @param {KvlConfig} config
 * @returns {*}
 */
function applyCompactingRules(data, config) {
  if (!isDict(data) || Object.keys(data).length === 0) return data;

  const keys = Object.keys(data);

  // Rule 1: Singleton empty key lifting
  if (keys.length === 1 && keys[0] === '') {
    const emptyValue = data[''];
    if (
      isDict(emptyValue) &&
      Object.keys(emptyValue).length > 0 &&
      Object.values(emptyValue).every(v => isDict(v))
    ) {
      return Object.entries(emptyValue).map(([k, v]) => ({ [k]: v }));
    }
    return emptyValue;
  }

  // Rule 2: Single empty child → string
  if (keys.length === 1) {
    const childKey = keys[0];
    const childValue = data[childKey];
    if (isDict(childValue) && Object.keys(childValue).length === 0 && childKey !== '') {
      return childKey;
    }
  }

  // Rule 3: All-empty-dict flattening to list
  if (Object.values(data).every(v => isDict(v) && Object.keys(v).length === 0)) {
    // Rule 4: Don't flatten if any key looks like a section header
    for (const key of keys) {
      if (typeof key === 'string' && isSectionHeader(key, config.separator)) {
        return data;
      }
    }
    return keys;
  }

  return data;
}

/**
 * Check if a value looks like a section header.
 * @param {string} value
 * @param {string} separator
 * @returns {boolean}
 */
function isSectionHeader(value, separator) {
  if (typeof value !== 'string' || !value.trim()) return false;
  return value.trim().startsWith(separator);
}

/**
 * @param {*} value
 * @returns {boolean}
 */
function isDict(value) {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}
