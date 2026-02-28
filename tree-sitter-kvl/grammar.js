/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "kvl",

  externals: $ => [
    $.key,
    $.continuation,
    $._error_sentinel,
  ],

  extras: _ => [],

  rules: {
    document: $ => repeat(choice(
      $.header,
      $.comment,
      $.list_item,
      $.pair,
      $.continuation,
      $._blank,
    )),

    // Header: #<sep> kvl <version> [options...]
    header: _ => token(prec(10, /[ \t]*#[^ \t\n]+[ \t]+kvl[ \t]+[0-9]+\.[0-9]+[^\n]*\n/)),

    // Comment: /= <text>
    comment: _ => token(prec(8, /[ \t]*\/=[^\n]*\n/)),

    // List item: <indent> <marker> <space> <value>
    list_item: $ => seq(
      /[ \t]*/,
      field("marker", $.list_marker),
      /[ \t]/,
      field("value", $.value),
      $._eol,
    ),

    list_marker: _ => token(prec(5, /[-+*]/)),

    // Key-value pair: <key> <separator> [<value>]
    // The key external token includes leading whitespace.
    pair: $ => seq(
      field("key", $.key),
      /[ \t]*/,
      field("separator", $.separator),
      optional(seq(
        /[ \t]+/,
        field("value", $.value),
      )),
      $._eol,
    ),

    separator: _ => token(prec(1, choice(":=", "->", "=", ":"))),

    // Value: everything after the separator to end of line.
    // Type detection (boolean, number, string) is handled by
    // highlight queries using #match? predicates.
    value: _ => /[^\n]+/,

    // Continuation: external scanner handles this.
    // Matches indented lines without a separator.

    // Terminals
    _blank: _ => /[ \t]*\n/,
    _eol: _ => /\n/,
  },
});
