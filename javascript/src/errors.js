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

/**
 * A warning or error diagnostic emitted during parsing.
 */
export class KvlDiagnostic {
  /**
   * @param {string} severity - "warning" or "error"
   * @param {string} code - e.g. "W001", "W002"
   * @param {string} message
   * @param {number} [line]
   */
  constructor(severity, code, message, line) {
    this.severity = severity;
    this.code = code;
    this.message = message;
    this.line = line ?? null;
  }
}
