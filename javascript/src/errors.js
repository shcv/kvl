/**
 * Base exception for KVL errors.
 */
export class KvlError extends Error {
  /** @param {string} message */
  constructor(message) {
    super(message);
    this.name = 'KvlError';
  }
}

/**
 * Error during parsing KVL text.
 */
export class KvlParseError extends KvlError {
  /**
   * @param {string} message
   * @param {number} [line]
   * @param {number} [column]
   */
  constructor(message, line, column) {
    const location = line != null
      ? ` at line ${line}${column != null ? `, column ${column}` : ''}`
      : '';
    super(`${message}${location}`);
    this.name = 'KvlParseError';
    this.line = line ?? null;
    this.column = column ?? null;
  }
}

/**
 * Error during serializing to KVL.
 */
export class KvlSerializeError extends KvlError {
  /** @param {string} message */
  constructor(message) {
    super(message);
    this.name = 'KvlSerializeError';
  }
}
