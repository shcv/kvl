#include "tree_sitter/parser.h"
#include <stdbool.h>

enum TokenType {
  KEY,
  CONTINUATION,
  ERROR_SENTINEL,
};

void *tree_sitter_kvl_external_scanner_create(void) { return NULL; }
void tree_sitter_kvl_external_scanner_destroy(void *payload) {}
unsigned tree_sitter_kvl_external_scanner_serialize(void *payload,
                                                    char *buffer) {
  return 0;
}
void tree_sitter_kvl_external_scanner_deserialize(void *payload,
                                                  const char *buffer,
                                                  unsigned length) {}

static inline bool is_eol(TSLexer *lexer) {
  return lexer->lookahead == '\n' || lexer->lookahead == '\0' ||
         lexer->eof(lexer);
}

static inline bool is_ws_or_eol(TSLexer *lexer) {
  int32_t c = lexer->lookahead;
  return c == ' ' || c == '\t' || c == '\n' || c == '\0' ||
         lexer->eof(lexer);
}

// Scan key content from current position. The lexer should be
// positioned at the first non-whitespace character of content.
// Returns: 1 if separator found (KEY), 0 if no separator (could be CONTINUATION)
static int scan_key_content(TSLexer *lexer) {
  bool has_content = false;
  bool in_trailing_ws = false;

  while (true) {
    int32_t c = lexer->lookahead;

    if (is_eol(lexer)) return has_content ? 0 : -1;

    // Escape sequences
    if (c == '\\') {
      if (in_trailing_ws) in_trailing_ws = false;
      lexer->advance(lexer, false);
      has_content = true;
      if (!is_eol(lexer)) lexer->advance(lexer, false);
      lexer->mark_end(lexer);
      continue;
    }

    // Whitespace (possibly trailing before separator)
    if (c == ' ' || c == '\t') {
      if (!in_trailing_ws && has_content) {
        lexer->mark_end(lexer);
        in_trailing_ws = true;
      }
      lexer->advance(lexer, false);
      continue;
    }

    // = separator
    if (c == '=') {
      lexer->advance(lexer, false);
      if (is_ws_or_eol(lexer)) {
        return has_content ? 1 : -1;
      }
      if (in_trailing_ws) in_trailing_ws = false;
      has_content = true;
      lexer->mark_end(lexer);
      continue;
    }

    // : or := separator
    if (c == ':') {
      lexer->advance(lexer, false);
      if (lexer->lookahead == '=') {
        lexer->advance(lexer, false);
        if (is_ws_or_eol(lexer)) {
          return has_content ? 1 : -1;
        }
        if (in_trailing_ws) in_trailing_ws = false;
        has_content = true;
        lexer->mark_end(lexer);
        continue;
      }
      if (is_ws_or_eol(lexer)) {
        return has_content ? 1 : -1;
      }
      if (in_trailing_ws) in_trailing_ws = false;
      has_content = true;
      lexer->mark_end(lexer);
      continue;
    }

    // -> separator
    if (c == '-') {
      lexer->advance(lexer, false);
      if (lexer->lookahead == '>') {
        lexer->advance(lexer, false);
        if (is_ws_or_eol(lexer)) {
          return has_content ? 1 : -1;
        }
        if (in_trailing_ws) in_trailing_ws = false;
        has_content = true;
        lexer->mark_end(lexer);
        continue;
      }
      if (in_trailing_ws) in_trailing_ws = false;
      has_content = true;
      lexer->mark_end(lexer);
      continue;
    }

    // Regular character
    if (in_trailing_ws) in_trailing_ws = false;
    lexer->advance(lexer, false);
    has_content = true;
    lexer->mark_end(lexer);
  }
}

static void consume_to_eol_incl(TSLexer *lexer) {
  while (!is_eol(lexer)) {
    lexer->advance(lexer, false);
  }
  if (lexer->lookahead == '\n') {
    lexer->advance(lexer, false);
  }
  lexer->mark_end(lexer);
}

bool tree_sitter_kvl_external_scanner_scan(void *payload, TSLexer *lexer,
                                           const bool *valid_symbols) {

  bool want_key = valid_symbols[KEY];
  bool want_cont = valid_symbols[CONTINUATION];

  if (!want_key && !want_cont) return false;

  // Skip leading whitespace (mark as skipped so it's not in the token)
  bool had_indent = false;
  while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
    lexer->advance(lexer, true);
    had_indent = true;
  }

  if (is_eol(lexer)) return false;

  // Let internal tokens handle comments (/=...) and headers (#...)
  if (lexer->lookahead == '/' || lexer->lookahead == '#') {
    return false;
  }

  // Check for list marker: single -, +, * followed by whitespace
  if (lexer->lookahead == '-' || lexer->lookahead == '+' ||
      lexer->lookahead == '*') {
    lexer->mark_end(lexer);
    int32_t marker_char = lexer->lookahead;
    lexer->advance(lexer, false);

    if (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
      // Looks like a list marker - let internal rules handle it
      return false;
    }

    // Not a list marker. First char is part of key content.
    // For '-', check if it's part of '->' separator
    if (marker_char == '-' && lexer->lookahead == '>') {
      lexer->advance(lexer, false);
      if (is_ws_or_eol(lexer)) {
        // -> separator with single-char key? Only valid if
        // there was content before. The marker char IS content.
        lexer->result_symbol = KEY;
        return want_key;
      }
      // Not a separator, continue scanning
      lexer->mark_end(lexer);
    } else {
      lexer->mark_end(lexer);
    }

    // Continue scanning from current position
    int result = scan_key_content(lexer);
    if (result == 1 && want_key) {
      lexer->result_symbol = KEY;
      return true;
    }
    if (result <= 0 && want_cont && had_indent) {
      consume_to_eol_incl(lexer);
      lexer->result_symbol = CONTINUATION;
      return true;
    }
    return false;
  }

  // Normal key scanning
  lexer->mark_end(lexer);
  int result = scan_key_content(lexer);

  if (result == 1 && want_key) {
    lexer->result_symbol = KEY;
    return true;
  }

  if (result <= 0 && want_cont && had_indent) {
    consume_to_eol_incl(lexer);
    lexer->result_symbol = CONTINUATION;
    return true;
  }

  return false;
}
